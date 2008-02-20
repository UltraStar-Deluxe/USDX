unit UAudioInput_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses
  Classes,
  SysUtils,
  URecord,
  UMusic;

implementation

uses
  UMain,
  UIni,
  ULog,
  UAudioCore_Bass,
  Windows,
  bass;

type
  TAudioInput_Bass = class(TAudioInputBase)
    public
      function GetName: String; override;
      function InitializeRecord: boolean; override;
      destructor Destroy; override;
  end;

  TBassInputDevice = class(TAudioInputDevice)
    public
      DeviceIndex: integer;  // index in TAudioInputProcessor.Device[]
      BassDeviceID: integer; // DeviceID used by BASS
      RecordStream: HSTREAM;

      procedure Start(); override;
      procedure Stop();  override;
  end;

var
  singleton_AudioInputBass : IAudioInput;


{ Global }

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
  AudioInputProcessor.HandleMicrophoneData(buffer, len,
      AudioInputProcessor.Device[Card]);
  Result := true;
end;


{ TBassInputDevice }

{*
 * Start input-capturing on this device.
 * TODO: call BASS_RecordInit only once
 *}
procedure TBassInputDevice.Start();
const
  captureFreq = 44100;
begin
  // recording already started -> stop first
  if (RecordStream <> 0) then
    Stop();

  // TODO: Call once. Otherwise it's to slow
  if not BASS_RecordInit(BassDeviceID) then
  begin
    Log.LogError('TBassInputDevice.Start: Error initializing device['+IntToStr(DeviceIndex)+']: ' +
                 TAudioCore_Bass.ErrorGetString());
    Exit;
  end;

  SampleRate := captureFreq;

  // capture in 44.1kHz/stereo/16bit and a 20ms callback period
  RecordStream := BASS_RecordStart(captureFreq, 2, MakeLong(0, 20),
                    @MicrophoneCallback, DeviceIndex);
  if (RecordStream = 0) then
  begin
    BASS_RecordFree;
    Exit;
  end;
end;

{*
 * Stop input-capturing on this device.
 *}
procedure TBassInputDevice.Stop();
begin
  if (RecordStream = 0) then
    Exit;
  // TODO: Don't free the device. Do this on close
  if (BASS_RecordSetDevice(BassDeviceID)) then
    BASS_RecordFree;
  RecordStream := 0;
end;


{ TAudioInput_Bass }

function  TAudioInput_Bass.GetName: String;
begin
  result := 'BASS_Input';
end;

function TAudioInput_Bass.InitializeRecord(): boolean;
var
  Descr:      PChar;
  SourceName: PChar;
  Flags:      integer;
  BassDeviceID: integer;
  BassDevice:   TBassInputDevice;
  DeviceIndex:  integer;
  SourceIndex:  integer;
begin
  result := false;

  DeviceIndex := 0;
  BassDeviceID := 0;
  SetLength(AudioInputProcessor.Device, 0);

  // checks for recording devices and puts them into an array
  while true do
  begin
    Descr := BASS_RecordGetDeviceDescription(BassDeviceID);
    if (Descr = nil) then
      break;

    // try to intialize the device
    if not BASS_RecordInit(BassDeviceID) then
    begin
      Log.LogStatus('Failed to initialize BASS Capture-Device['+inttostr(BassDeviceID)+']',
                    'TAudioInput_Bass.InitializeRecord');
    end
    else
    begin
      SetLength(AudioInputProcessor.Device, DeviceIndex+1);

      // TODO: free object on termination
      BassDevice := TBassInputDevice.Create();
      AudioInputProcessor.Device[DeviceIndex] := BassDevice;

      BassDevice.DeviceIndex := DeviceIndex;
      BassDevice.BassDeviceID := BassDeviceID;
      BassDevice.Description := UnifyDeviceName(Descr, DeviceIndex);

      // get input sources
      SourceIndex := 0;
      BassDevice.MicInput := 0;

      // process each input
      while true do
      begin
        SourceName := BASS_RecordGetInputName(SourceIndex);
        if (SourceName = nil) then
          break;

        SetLength(BassDevice.Source, SourceIndex+1);
        BassDevice.Source[SourceIndex].Name :=
          UnifyDeviceSourceName(SourceName, BassDevice.Description);

        // set mic index
        Flags := BASS_RecordGetInput(SourceIndex);
        if ((Flags <> -1) and ((Flags and BASS_INPUT_TYPE_MIC) <> 0)) then
        begin
          BassDevice.MicInput := SourceIndex;
        end;

        Inc(SourceIndex);
      end;

      //Writeln('BASS_RecordFree');
      // FIXME: this call hangs in FPC (windows) every 2nd time USDX is called.
      //   Maybe because the sound-device was not released properly?
      BASS_RecordFree;
      //Writeln('BASS_RecordFree - Done');

      Inc(DeviceIndex);
    end;
    
    Inc(BassDeviceID);
  end;

  result := true;
end;

destructor TAudioInput_Bass.Destroy;
begin
  inherited;
end;


initialization
  singleton_AudioInputBass := TAudioInput_Bass.create();
  AudioManager.add( singleton_AudioInputBass );

finalization
  AudioManager.Remove( singleton_AudioInputBass );

end.
