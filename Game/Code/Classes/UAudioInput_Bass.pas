unit UAudioInput_Bass;

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
  TAudioInput_Bass = class( TInterfacedObject, IAudioInput)
    private
    public
      function  GetName: String;

      {IAudioInput interface}
      procedure InitializeRecord;

      procedure CaptureStart;
      procedure CaptureStop;
      procedure CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
      procedure StopCard(Card: byte);
  end;

  TBassSoundCard = class(TGenericSoundCard)
    RecordStream: HSTREAM;
  end;

var
  singleton_AudioInputBass : IAudioInput;


function  TAudioInput_Bass.GetName: String;
begin
  result := 'BASS_Input';
end;

procedure TAudioInput_Bass.InitializeRecord;
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
procedure TAudioInput_Bass.CaptureStart;
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
procedure TAudioInput_Bass.CaptureStop;
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
procedure TAudioInput_Bass.CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
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
procedure TAudioInput_Bass.StopCard(Card: byte);
begin
  BASS_RecordSetDevice(Card);
  BASS_RecordFree;
end;


initialization
  singleton_AudioInputBass := TAudioInput_Bass.create();
  AudioManager.add( singleton_AudioInputBass );

finalization
  AudioManager.Remove( singleton_AudioInputBass );

end.
