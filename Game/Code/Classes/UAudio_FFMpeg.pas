unit UAudio_FFMpeg;

(*******************************************************************************

This unit is primarily based upon -
    http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
    
    and tutorial03.c

    http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html

*******************************************************************************)

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
     SDL,       // Used for Audio output Interface
     avcodec,   // FFMpeg Audio file decoding
     avformat,
     avutil,
     ULog,
     UMusic;

type
  TPacketQueue = record
    first_pkt  ,
    last_pkt   : pAVPacketList;
    nb_packets : integer;
    size       : integer;
    mutex      : pSDL_mutex;
    cond       : pSDL_cond;
  end;
  pPacketQueue = ^TPacketQueue;

  function  packet_queue_put(var aPacketQueue : TPacketQueue; var AVPacket : TAVPacket): integer;
  function  packet_queue_get(var aPacketQueue : TPacketQueue; var AVPacket : TAVPacket; block : integer ): integer;
  procedure packet_queue_init( var aPacketQueue : TPacketQueue );
  procedure audio_callback( userdata: Pointer; stream: PUInt8; len: Integer ); cdecl;
  function  audio_decode_frame(aCodecCtx : TAVCodecContext; aAudio_buf : PUInt8; buf_size: integer): integer;

var
  singleton_MusicFFMpeg : IAudioPlayback = nil;

var
  audioq        : TPacketQueue;
  quit          : integer       = 0;
//  faudio_buf     : array[ 0 .. 0 ] of byte; //pUInt8{$ifndef fpc};{$else} = nil;{$endif}
//  audio_buf     : array[ 0 .. AVCODEC_MAX_AUDIO_FRAME_SIZE ] of byte; //pUInt8{$ifndef fpc};{$else} = nil;{$endif}

type
  Taudiobuff = array[ 0 .. AVCODEC_MAX_AUDIO_FRAME_SIZE ] of byte;
  PAudioBuff = ^Taudiobuff;

implementation

uses
     {$IFDEF FPC}
     lclintf,
     libc,
     {$ENDIF}
//     URecord,
     UIni,
     UMain,
     UThemes;

//var
//  singleton_MusicFFMpeg : IAudioPlayback = nil;


const
  RecordSystem = 1;
  SDL_AUDIO_BUFFER_SIZE = 1024;
  

type
  TMPModes = (mpNotReady, mpStopped, mpPlaying, mpRecording, mpSeeking, mpPaused, mpOpen);



const
  ModeStr:  array[TMPModes] of string = ('Not ready', 'Stopped', 'Playing', 'Recording', 'Seeking', 'Paused', 'Open');



type
    TAudio_ffMpeg = class( TInterfacedObject, IAudioPlayback )
    private

      BassStart:          hStream;
      BassBack:           hStream;
      BassSwoosh:         hStream;
      BassChange:         hStream;
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

      function find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;
    public
//      Bass: hStream;
      constructor create();
      function  GetName: String;
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
//      procedure CaptureStart;
//      procedure CaptureStop;
//      procedure CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
      procedure StopCard(Card: byte);
      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
end;

constructor TAudio_ffMpeg.create();
begin
//  writeln( 'UVideo_FFMpeg - av_register_all' );
  av_register_all;
end;

function TAudio_ffMpeg.find_stream_ids( const aFormatCtx : PAVFormatContext; Out aFirstVideoStream, aFirstAudioStream : integer ): boolean;
var
  i : integer;
  st : pAVStream;
begin
  // Find the first video stream
  aFirstAudioStream := -1;
  aFirstVideoStream := -1;

  i := 0;
  while ( i < aFormatCtx.nb_streams ) do
  begin
//    writeln( ' aFormatCtx.streams[i] : ' + inttostr( i ) );
    st := aFormatCtx.streams[i];

    if(st.codec.codec_type = CODEC_TYPE_VIDEO ) AND
      (aFirstVideoStream < 0) THEN
    begin
//      writeln( 'Found Video Stream' );
      aFirstVideoStream := i;
    end;

    if ( st.codec.codec_type = CODEC_TYPE_AUDIO ) AND
       ( aFirstAudioStream < 0) THEN
    begin
//      writeln( 'Found Audio Stream' );
      aFirstAudioStream := i;
    end;

    inc( i );
  end; // while

  result := (aFirstAudioStream > -1) OR
            (aFirstVideoStream > -1) ;  // Didn't find any streams stream
end;


function  TAudio_ffMpeg.GetName: String;
begin
  result := 'FFMpeg';
end;

procedure TAudio_ffMpeg.InitializePlayback;
var
//  Pet:  integer;
  S:    integer;
begin

//  LoadSoundFromFile(BassStart,  SoundPath + 'Green Day - American Idiot.mp3');
  
(*
  LoadSoundFromFile(BassStart,  SoundPath + 'Common start.mp3');
  LoadSoundFromFile(BassBack,   SoundPath + 'Common back.mp3');
  LoadSoundFromFile(BassSwoosh, SoundPath + 'menu swoosh.mp3');
  LoadSoundFromFile(BassChange, SoundPath + 'select music change music 50.mp3');
  LoadSoundFromFile(BassOption, SoundPath + 'option change col.mp3');
  LoadSoundFromFile(BassClick,  SoundPath + 'rimshot022b.mp3');

  LoadSoundFromFile(BassDrum,   SoundPath + 'bassdrumhard076b.mp3');
  LoadSoundFromFile(BassHihat,  SoundPath + 'hihatclosed068b.mp3');
  LoadSoundFromFile(BassClap,   SoundPath + 'claps050b.mp3');
*)

//  LoadSoundFromFile(BassShuffle, SoundPath + 'Shuffle.mp3');

//  Log.BenchmarkEnd(4);
//  Log.LogBenchmark('--> Loading Sounds', 4);

end;


procedure TAudio_ffMpeg.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application


  // TODO : jb_linux replace with something other than bass
(*
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
*)
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
//  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
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
//    Bass := Bass_StreamCreateFile(false, pchar(Name), 0, 0, 0);
    
    Loaded := true;
    //Set Max Volume
//    SetMusicVolume (100);
  end;

  Result := Loaded;
end;

procedure TAudio_ffMpeg.Rewind;
begin
  if Loaded then
  begin
  end;
end;

procedure TAudio_ffMpeg.MoveTo(Time: real);
var
  bytes:    integer;
begin
  // TODO : jb_linux replace with something other than bass
//  bytes := BASS_ChannelSeconds2Bytes(Bass, Time);
//  BASS_ChannelSetPosition(Bass, bytes);
end;

procedure TAudio_ffMpeg.Play;
begin
(*
  // TODO : jb_linux replace with something other than bass
  if Loaded then
  begin
    if Loop then
      BASS_ChannelPlay(Bass, True); // start from beginning... actually bass itself does not loop, nor does this TAudio_ffMpeg Class

    BASS_ChannelPlay(Bass, False); // for setting position before playing
  end;
*)
end;

procedure TAudio_ffMpeg.Pause; //Pause Mod
begin
(*
  // TODO : jb_linux replace with something other than bass
  if Loaded then begin
    BASS_ChannelPause(Bass); // Pauses Song
  end;
*)
end;

procedure TAudio_ffMpeg.Stop;
begin
//  Bass_ChannelStop(Bass);
end;

procedure TAudio_ffMpeg.Close;
begin
//  Bass_StreamFree(Bass);
end;

function TAudio_ffMpeg.Length: real;
var
  bytes:    integer;
begin
  Result := 60;
(*
  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetLength(Bass);
  Result := BASS_ChannelBytes2Seconds(Bass, bytes);
*)
end;

function TAudio_ffMpeg.getPosition: real;
var
  bytes:    integer;
begin
  Result := 0;

(*
  // TODO : jb_linux replace with something other than bass
  bytes  := BASS_ChannelGetPosition(BASS);
  Result := BASS_ChannelBytes2Seconds(BASS, bytes);
*)
end;

function TAudio_ffMpeg.Finished: boolean;
begin
  Result := false;

(*
  // TODO : jb_linux replace with something other than bass
  if BASS_ChannelIsActive(BASS) = BASS_ACTIVE_STOPPED then
  begin
    Result := true;
  end;
*)
end;

procedure TAudio_ffMpeg.PlayStart;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassStart, True);
end;

procedure TAudio_ffMpeg.PlayBack;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassBack, True);// then
end;

procedure TAudio_ffMpeg.PlaySwoosh;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassSwoosh, True);


end;

procedure TAudio_ffMpeg.PlayChange;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassChange, True);
end;

procedure TAudio_ffMpeg.PlayOption;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassOption, True);
end;

procedure TAudio_ffMpeg.PlayClick;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassClick, True);
end;

procedure TAudio_ffMpeg.PlayDrum;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassDrum, True);
end;

procedure TAudio_ffMpeg.PlayHihat;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassHihat, True);
end;

procedure TAudio_ffMpeg.PlayClap;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassClap, True);
end;

procedure TAudio_ffMpeg.PlayShuffle;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelPlay(BassShuffle, True);
end;

procedure TAudio_ffMpeg.StopShuffle;
begin
  // TODO : jb_linux replace with something other than bass
//  BASS_ChannelStop(BassShuffle);
end;

procedure TAudio_ffMpeg.StopCard(Card: byte);
begin

  // TODO : jb_linux replace with something other than bass
//  BASS_RecordSetDevice(Card);
//  BASS_RecordFree;
end;

function audio_decode_frame(aCodecCtx : TAVCodecContext; aAudio_buf : PUInt8; buf_size: integer): integer;
var
  pkt             : TAVPacket;
  audio_pkt_data  : pchar;//PUInt8 = nil;
  audio_pkt_size  : integer;
  len1            ,
  data_size       : integer;
begin
  {$ifdef win32}
    FillChar(pkt, sizeof(pkt), #0);
  {$else}
    memset(@pkt, 0, sizeof(pkt));   // todo : jb memset
  {$endif}

  audio_pkt_data := nil;
  audio_pkt_size := 0;

  while true do
  begin
  
    while ( audio_pkt_size > 0 ) do
    begin
//      writeln( 'got audio packet' );
      data_size := buf_size;

      len1 := -1;

      if aAudio_buf <> nil  then
      begin
//        writeln( 'pre avcodec_decode_audio' );
        {$ifdef fpc}
          len1 := avcodec_decode_audio(@aCodecCtx, PWord( aAudio_buf ), data_size, audio_pkt_data, audio_pkt_size); // Todo.. should be avcodec_decode_audio2 but this wont link on my ubuntu box.
        {$else}
          len1 := avcodec_decode_audio(@aCodecCtx, Pointer( aAudio_buf ), data_size, audio_pkt_data, audio_pkt_size); // Todo.. should be avcodec_decode_audio2 but this wont link on my ubuntu box.
        {$endif}
//        writeln( 'post avcodec_decode_audio' );        

      end;

//      writeln('avcodec_decode_audio');

      if(len1 < 0) then
      begin
	      //* if error, skip frame */
//       	writeln( 'Skip audio frame' );
        audio_pkt_size := 0;
       	break;
      end;
      
      audio_pkt_data := audio_pkt_data + len1;
      audio_pkt_size := audio_pkt_size + len1;
      
      if (data_size <= 0) then
      begin
    	  //* No data yet, get more frames */
     	  continue;
      end;
      
      //* We have data, return it and come back for more later */
      result := data_size;
      exit;
    end;

    if ( pkt.data <> nil ) then
      av_free_packet( pkt );

    if ( quit <> 0 ) then
    begin
      result := -1;
      exit;
    end;

    if (packet_queue_get(audioq, pkt, 1) < 0) then
    begin
      result := -1;
      exit;
    end;


    audio_pkt_data := pchar( pkt.data );
    audio_pkt_size := pkt.size;
//    writeln( 'Audio Packet Size - ' + inttostr(audio_pkt_size) );
  end;
end;

procedure audio_callback( userdata: Pointer; stream: PUInt8; len: Integer );
var
  audio_buf_index : cardinal; // static unsigned int audio_buf_index = 0;
  audio_buf_size  : cardinal; // static unsigned int audio_buf_size = 0;
  audio_size      ,
  len1            : integer;
  aCodecCtx       : TAVCodecContext;

  lSrc            : pointer;

  // this is used to emulate ...... static uint8_t audio_buf[(AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) / 2];
  lAudio_buf_data : Taudiobuff;  // This created the memory we need
  laudio_buf      : PAudioBuff;  // this makes it easy to work with.. since its the pointer to that memeory everywhere                                     
begin
  laudio_buf      := @lAudio_buf_data ;

  aCodecCtx       := pAVCodecContext(userdata)^;
  audio_size      := -1;
  audio_buf_index := 0;
  audio_buf_size  := 0;

  while (len > 0) do
  begin
    if(audio_buf_index >= audio_buf_size) then
    begin
      // We have already sent all our data; get more */
      audio_size := audio_decode_frame(aCodecCtx, pUInt8( laudio_buf ), sizeof(laudio_buf));

      if(audio_size < 0) then
      begin
      	// If error, output silence */
      	audio_buf_size := 1024; // arbitrary?

        {$ifdef win32}
          FillChar(laudio_buf, audio_buf_size, #0);
        {$else}
        	memset(laudio_buf, 0, audio_buf_size);   // todo : jb memset
        {$endif}
      end
      else
      begin
      	audio_buf_size := audio_size;
      end;

      audio_buf_index := 0;  // Todo : jb - SegFault ?
    end;
    
    len1 := audio_buf_size - audio_buf_index;

    if (len1 > len) then
      len1 := len;


    {$ifdef win32}
      lSrc := PUInt8( integer( laudio_buf ) + audio_buf_index );
      CopyMemory(stream, lSrc , len1);
    {$else}
      memcpy(stream, PUInt8( laudio_buf ) + audio_buf_index , len1);
    {$endif}

    len             := len             - len1;
    stream^         := stream^         + len1;
    audio_buf_index := audio_buf_index + len1;
  end;
end;

function TAudio_ffMpeg.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
var
  L             : Integer;
  pFormatCtx    : PAVFormatContext;
  lVidStreamID  ,
  lAudStreamID  : Integer;
  aCodecCtx     : pAVCodecContext;
  wanted_spec   ,
  spec          : TSDL_AudioSpec;
  lAudioStream  : pAVStream;
  aCodec        : pAVCodec;
  i             : integer;
  packet        : TAVPacket;
  event         : TSDL_Event;
begin
  result := false;

  if FileExists(Name) then
  begin
//    writeln('Loading Sound: "' + Name + '"', 'LoadSoundFromFile');
    
  // Open video file
  if (av_open_input_file(pFormatCtx, pchar(Name), nil, 0, nil) > 0) then
    exit;

  // Retrieve stream information
  if (av_find_stream_info(pFormatCtx)<0) then
    exit;

  dump_format(pFormatCtx, 0, pchar(Name), 0);
  
  if not find_stream_ids( pFormatCtx, lVidStreamID, lAudStreamID ) then
    exit;
    
//  writeln( 'done searching for stream ids' );
    
  if lAudStreamID > -1 then
  begin
//    writeln( 'Audio Stream ID is : '+ inttostr( lAudStreamID ) );

    lAudioStream := pFormatCtx.streams[lAudStreamID];
    aCodecCtx := lAudioStream.codec;

    // Set audio settings from codec info
    wanted_spec.freq     := aCodecCtx.sample_rate;
    wanted_spec.format   := AUDIO_S16SYS;
    wanted_spec.channels := aCodecCtx.channels;
    wanted_spec.silence  := 0;
    wanted_spec.samples  := SDL_AUDIO_BUFFER_SIZE;
    wanted_spec.callback := audio_callback;
    wanted_spec.userdata := aCodecCtx;
  end;
  
  if (SDL_OpenAudio(@wanted_spec, @spec) < 0) then
  begin
    writeln('SDL_OpenAudio: '+SDL_GetError());
    exit
  end;

//  writeln( 'SDL opened audio device' );

  aCodec := avcodec_find_decoder(aCodecCtx.codec_id);
  if (aCodec = nil) then
  begin
    writeln('Unsupported codec!');
    exit;
  end;

  avcodec_open(aCodecCtx, aCodec);

//  writeln( 'Opened the codec' );
  
  packet_queue_init( audioq );
  SDL_PauseAudio(0);
  
//  writeln( 'SDL_PauseAudio' );
  
  i := 0;
  while (av_read_frame(pFormatCtx, packet) >= 0) do
  begin
//    writeln( 'ffmpeg - av_read_frame' );
    
    if (packet.stream_index = lAudStreamID ) then
    begin
//      writeln( 'packet_queue_put' );
      packet_queue_put(audioq, packet);
    end
    else
    begin
      av_free_packet(packet);
    end;


    // Free the packet that was allocated by av_read_frame
    SDL_PollEvent(@event);

(*
    if  event.type_ = SDL_QUIT the
    begin
      quit := 1;
      SDL_Quit();
    end
    else
      break;
*)

  end;

//  halt(0);

  // Close the codec
//  avcodec_close(aCodecCtx);

  // Close the video file
//  av_close_input_file(pFormatCtx);

(*
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
*)
    
  end
  else
  begin
    writeln('Sound not found: "' + Name + '"', 'LoadSoundFromFile');
    exit;
  end;

end;

procedure packet_queue_init(var aPacketQueue : TPacketQueue );
begin
  {$ifdef win32}
    FillChar(aPacketQueue, sizeof(TPacketQueue), #0);
  {$else}
    memset(@aPacketQueue, 0, sizeof(TPacketQueue));
  {$endif}
  
  aPacketQueue.mutex := SDL_CreateMutex();
  aPacketQueue.cond  := SDL_CreateCond();
end;

function packet_queue_put(var aPacketQueue : TPacketQueue; var AVPacket : TAVPacket): integer;
var
  pkt1 : pAVPacketList;
begin
  result := -1;
  
//  writeln( 'TAudio_ffMpeg.packet_queue_put' );
  
  if av_dup_packet(@AVPacket) < 0 then
    exit;
    
  pkt1 := av_malloc(sizeof(TAVPacketList));
  if (pkt1 = nil) then
    exit;
   
  pkt1.pkt  := AVPacket;
  pkt1.next := nil;


  SDL_LockMutex( aPacketQueue.mutex );
  try

    if (aPacketQueue.last_pkt = nil) then
      aPacketQueue.first_pkt := pkt1
    else
      aPacketQueue.last_pkt.next := pkt1;
      
    aPacketQueue.last_pkt := pkt1;
    inc( aPacketQueue.nb_packets );
    
    aPacketQueue.size := aPacketQueue.size + pkt1.pkt.size;
    SDL_CondSignal(aPacketQueue.cond);

  finally
    SDL_UnlockMutex( aPacketQueue.mutex );
  end;

  result := 0;
end;

function packet_queue_get(var aPacketQueue : TPacketQueue; var AVPacket : TAVPacket; block : integer ): integer;
var
  pkt1 : pAVPacketList;
begin
  result := -1;
//  writeln( 'packet_queue_get' );
  
  SDL_LockMutex(aPacketQueue.mutex);
  try
    while true do
    begin

      if (quit <> 0) then
        exit;

      pkt1 := aPacketQueue.first_pkt;
      
      if ( pkt1 <> nil ) then
      begin
        aPacketQueue.first_pkt := pkt1.next;
        
        if (aPacketQueue.first_pkt = nil ) then
      	  aPacketQueue.last_pkt := nil;
         
        dec(aPacketQueue.nb_packets);
        
        aPacketQueue.size := aPacketQueue.size - pkt1.pkt.size;
        
        AVPacket := pkt1.pkt;

        av_free(pkt1);

        result := 1;
        break;
      end
      else
      if (block = 0) then
      begin
        result := 0;
        break;
      end
      else
      begin
        SDL_CondWait(aPacketQueue.cond, aPacketQueue.mutex);
      end;
    end;
  finally
    SDL_UnlockMutex(aPacketQueue.mutex);
  end;
end;


//Equalizer
function TAudio_ffMpeg.GetFFTData: TFFTData;
var
  Data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
//  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
end;

function TAudio_ffMpeg.LoadCustomSound(const Filename: String): Cardinal;
var
  S: hStream;
  I: Integer;
  F: String;
begin
(*
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
*)
end;

procedure TAudio_ffMpeg.PlayCustomSound(const Index: Cardinal );
begin
//  if Index <= High(CustomSounds) then
//    BASS_ChannelPlay(CustomSounds[Index].Handle, True);
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
