// UltraStar Deluxe - Karaoke Game
// SPDX-License-Identifier: GPL-2.0-or-later

unit UAudioInput_SDL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}

uses
  Classes,
  SysUtils,
  UMusic;

implementation

uses
  sdl2,
  ctypes,
  math,
  UIni,
  ULog,
  UPulseSources,
  URecord;

const
  DEVICE_CHECK_INTERVAL   = 500;   // ms between two health checks of the open devices
  DEVICE_REOPEN_INTERVAL  = 1000;  // ms between two attempts to reopen a lost device
  DEVICE_SILENCE_TIMEOUT  = 1500;  // ms of digital silence after which a device counts as lost
  DEVICE_SILENCE_MAX      = 12000; // upper limit for the silence timeout (see CheckHealth)

type
  TAudioInput_SDL = class(TAudioInputBase)
    private
      Initialized:   boolean;
      LastCheckTick: cardinal;
      function EnumDevices(): boolean;
    public
      function GetName: string; override;
      function InitializeRecord(ScanMode: TAudioInputScanMode): boolean; override;
      function FinalizeRecord: boolean; override;
      procedure CheckDevices; override;
  end;

  TSDLInputDevice = class(TAudioInputDevice)
    private
      DevID:   TSDL_AudioDeviceID;
      UseName: boolean;
      SDLName: UTF8String;      // name SDL currently uses for this device
      Wanted:  boolean;         // capturing was requested (between Start and Stop)
      Port:    UTF8String;      // where the device is plugged in ('' if unknown)
      LastOpenTick:    cardinal;
      LastSignalTick:  cardinal; // last time a non-silent buffer arrived (set by audio thread)
      SignalSinceOpen: boolean;  // a non-silent buffer arrived since the device was opened
      SilenceTimeout:  cardinal;
      SilenceLogged:   boolean;  // the current period of silence was logged already
      OpenError:       string;

      function ResolveSDLName(): boolean;
      function Open(): boolean;
      procedure CheckHealth();
    public
      function Start(): boolean; override;
      function Stop():  boolean; override;

      function GetVolume(): single;        override;
      procedure SetVolume(Volume: single); override;
  end;

procedure MicrophoneCallback(inputDevice: TSDLInputDevice; input: pointer; len: cint); cdecl;
var
  i:      integer;
  sample: PSmallInt;
begin
  // A device that got disconnected (e.g. a USB port reset) keeps calling us,
  // but the buffers only contain zeros. A working microphone always has some
  // noise, so remember when we last saw real data.
  sample := input;
  for i := 1 to len div SizeOf(SmallInt) do
  begin
    if sample^ <> 0 then
    begin
      inputDevice.LastSignalTick := SDL_GetTicks;
      inputDevice.SignalSinceOpen := true;
      break;
    end;
    Inc(sample);
  end;

  AudioInputProcessor.HandleMicrophoneData(input, len, inputDevice);
end;

// removes the " (2)" SDL appends to devices with identical names
function StripDuplicateSuffix(const name: UTF8String): UTF8String;
var
  i: integer;
begin
  Result := name;
  i := Length(name);
  if (i < 4) or (name[i] <> ')') then
    Exit;
  Dec(i);
  while (i > 0) and (name[i] in ['0'..'9']) do
    Dec(i);
  if (i > 1) and (i < Length(name) - 1) and (name[i] = '(') and (name[i-1] = ' ') then
    Result := Copy(name, 1, i - 2);
end;

type
  TUTF8StringArray = array of UTF8String;

{*
 * Lists SDL's recording devices and, where possible, the port each of them
 * is plugged into ('' if unknown).
 * SDL does not tell us the port, the sound server does. Both list the devices
 * in the same order, so the n-th SDL device with a certain description is the
 * n-th device with that description of the sound server.
 * Returns true if the sound server could be asked.
 *}
function ListCaptureDevices(out names, ports: TUTF8StringArray): boolean;
var
  i, j, k: integer;
  count:   integer;
  sdlDev:  PChar;
  sources: TPulseSourceArray;
  base:    UTF8String;
  matches: array of integer;
  usable:  boolean;
begin
  Result := false;

  // this also refreshes SDL's device list
  count := SDL_GetNumAudioDevices(1);
  SetLength(names, 0);
  for i := 0 to count - 1 do
  begin
    sdlDev := SDL_GetAudioDeviceName(i, 1);
    if sdlDev = nil then
      continue;
    SetLength(names, Length(names) + 1);
    names[High(names)] := sdlDev;
  end;

  SetLength(ports, Length(names));
  for i := 0 to High(ports) do
    ports[i] := '';

  if StrPas(SDL_GetCurrentAudioDriver) <> 'pulseaudio' then
    Exit;
  if not GetPulseSources(sources) then
    Exit;
  Result := true;

  for i := 0 to High(names) do
  begin
    base := StripDuplicateSuffix(names[i]);

    // handle every description once, at its first device
    usable := true;
    for j := 0 to i - 1 do
      if StripDuplicateSuffix(names[j]) = base then
        usable := false;
    if not usable then
      continue;

    SetLength(matches, 0);
    for j := 0 to High(sources) do
    begin
      if sources[j].Description <> base then
        continue;
      // the port has to be known and must identify the device
      if sources[j].BusPath = '' then
        usable := false;
      for k := 0 to High(matches) do
        if sources[matches[k]].BusPath = sources[j].BusPath then
          usable := false;
      SetLength(matches, Length(matches) + 1);
      matches[High(matches)] := j;
    end;

    count := 0;
    for j := i to High(names) do
      if StripDuplicateSuffix(names[j]) = base then
        Inc(count);

    // while a device is (dis)appearing both lists may differ for a moment
    if (not usable) or (count <> Length(matches)) then
      continue;

    k := 0;
    for j := i to High(names) do
    begin
      if StripDuplicateSuffix(names[j]) = base then
      begin
        ports[j] := sources[matches[k]].BusPath;
        Inc(k);
      end;
    end;
  end;
end;

// Form of a bus path used in the device name, e.g. "09:00.0-usb-0:3:1.0".
// The controller stays part of it: the same port number exists once per
// USB controller.
function PortLabel(const port: UTF8String): UTF8String;
const
  PCI_PREFIX = 'pci-0000:';
begin
  Result := port;
  if Copy(Result, 1, Length(PCI_PREFIX)) = PCI_PREFIX then
    Delete(Result, 1, Length(PCI_PREFIX));
end;

function FindDeviceConfig(const name: UTF8String): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(Ini.InputDeviceConfig) do
  begin
    if Ini.InputDeviceConfig[i].Name = name then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

{*
 * Makes sure SDLName is a name SDL currently knows.
 * Identical devices only differ by a number SDL appends in the order they
 * show up. After a reconnect "Mic (2)" might come back as "Mic (3)".
 * If we know the port of the device we look for the device at that port.
 * Otherwise, if our name is gone, we take a device with the same base name
 * that is not used by another input device.
 *}
function TSDLInputDevice.ResolveSDLName(): boolean;
var
  i, j:    integer;
  names:   TUTF8StringArray;
  ports:   TUTF8StringArray;
  other:   TAudioInputDevice;
  taken:   boolean;
begin
  Result := false;

  if ListCaptureDevices(names, ports) and (Port <> '') then
  begin
    for i := 0 to High(names) do
    begin
      if ports[i] = Port then
      begin
        if names[i] <> SDLName then
          Log.LogStatus('InputDevice "' + Self.Name + '" is now known to SDL as "' + names[i] + '"', 'SDL');
        SDLName := names[i];
        Result := true;
        Exit;
      end;
    end;
    // nothing is connected to our port at the moment
    Exit;
  end;

  for i := 0 to High(names) do
  begin
    if names[i] = SDLName then
    begin
      Result := true;
      Exit;
    end;
  end;

  for i := 0 to High(names) do
  begin
    if StripDuplicateSuffix(names[i]) <> StripDuplicateSuffix(SDLName) then
      continue;

    taken := false;
    for j := 0 to High(AudioInputProcessor.DeviceList) do
    begin
      other := AudioInputProcessor.DeviceList[j];
      if assigned(other) and (other <> Self) and (other is TSDLInputDevice) and
         (TSDLInputDevice(other).SDLName = names[i]) then
        taken := true;
    end;

    if not taken then
    begin
      Log.LogStatus('InputDevice "' + Self.Name + '" is now known to SDL as "' + names[i] + '"', 'SDL');
      SDLName := names[i];
      Result := true;
      Exit;
    end;
  end;
end;

function TSDLInputDevice.Open(): boolean;
var
  devName: PChar;
  spec:    TSDL_AudioSpec;
begin
  Result := false;
  LastOpenTick := SDL_GetTicks;
  SignalSinceOpen := false;

  devName := nil;
  if UseName then
  begin
    if not ResolveSDLName() then
    begin
      OpenError := 'device not found';
      Exit;
    end;
    devName := PChar(SDLName);
  end;

  FillChar(spec, SizeOf(spec), 0);
  with spec do
  begin
    freq := Round(AudioFormat.SampleRate);
    format := AUDIO_S16SYS;
    channels := AudioFormat.Channels;
    callback := @MicrophoneCallback;
    userdata := pointer(Self);

    samples := 0;
    if Ini.InputDeviceConfig[CfgIndex].Latency > 0 then
      samples := 1 shl Round(Max(Log2(freq / 1000 * Ini.InputDeviceConfig[CfgIndex].Latency), 0));
  end;

  DevID := SDL_OpenAudioDevice(devName, 1, @spec, @spec, 0);
  if DevID = 0 then
  begin
    OpenError := SDL_GetError;
    Exit;
  end;

  if Ini.InputDeviceConfig[CfgIndex].Latency > 0 then
    Log.LogStatus('InputDevice "' + Name + '" opened with ' +
                  IntToStr(spec.samples) + ' samples (' +
                  IntToStr(round(spec.samples * 1000 / spec.freq)) +
                  'ms) buffer', 'SDL');
  LastSignalTick := SDL_GetTicks;
  SDL_PauseAudioDevice(DevID, 0);
  Result := true;
end;

function TSDLInputDevice.Start(): boolean;
begin
  Wanted := true;
  SilenceTimeout := DEVICE_SILENCE_TIMEOUT;
  SilenceLogged := false;

  if DevID = 0 then
  begin
    Disconnected := false;
    if not Open() then
    begin
      // CheckHealth keeps trying while capturing is active
      Disconnected := true;
      Log.LogError('Could not open input device "' + Name + '": ' + OpenError, 'SDL');
    end;
  end;

  Result := (DevID > 0);
end;

function TSDLInputDevice.Stop(): boolean;
begin
  Wanted := false;
  Disconnected := false;
  if DevID > 0 then
    SDL_CloseAudioDevice(DevID);
  DevID := 0;
  Result := true;
end;

{*
 * Detects a device that silently died while capturing and reopens it.
 * Called regularly from the main thread.
 *
 * A device that is gone is reported as disconnected. A device that is still
 * there but only delivers zeros is reopened too, as SDL keeps feeding silence
 * to a stream whose device was gone for a moment. This is not reported,
 * because a muted microphone looks exactly the same.
 *}
procedure TSDLInputDevice.CheckHealth();
var
  Stopped:   boolean;
  SilentFor: int64;

  procedure MarkDisconnected();
  begin
    if not Disconnected then
    begin
      Disconnected := true;
      Log.LogError('Input device "' + Name + '" got disconnected, trying to reconnect', 'SDL');
    end;
  end;

begin
  if not Wanted then
    Exit;

  if DevID > 0 then
  begin
    Stopped := (SDL_GetAudioDeviceStatus(DevID) = SDL_AUDIO_STOPPED);
    SilentFor := int64(SDL_GetTicks) - int64(LastSignalTick);

    if (not Stopped) and (SilentFor < SilenceTimeout) then
    begin
      if SignalSinceOpen then
      begin
        SilenceTimeout := DEVICE_SILENCE_TIMEOUT;
        SilenceLogged := false;
        if Disconnected then
        begin
          Disconnected := false;
          Log.LogStatus('InputDevice "' + Name + '" is delivering audio again', 'SDL');
        end;
      end;
      Exit;
    end;

    if Stopped then
      MarkDisconnected()
    else
    begin
      if not SilenceLogged then
      begin
        SilenceLogged := true;
        Log.LogStatus('InputDevice "' + Name + '" only delivers silence, reopening it', 'SDL');
      end;
      // A muted microphone stays silent. Do not reopen it over and over at
      // a fast pace, each attempt costs some milliseconds.
      if SilenceTimeout < DEVICE_SILENCE_MAX then
        SilenceTimeout := SilenceTimeout * 2;
    end;

    SDL_CloseAudioDevice(DevID);
    DevID := 0;
  end
  else if int64(SDL_GetTicks) - int64(LastOpenTick) < DEVICE_REOPEN_INTERVAL then
    Exit;

  if Open() then
  begin
    if Disconnected then
      Log.LogStatus('InputDevice "' + Name + '" reopened as SDL device "' + SDLName + '"', 'SDL');
  end
  else
    MarkDisconnected();
end;

function TSDLInputDevice.GetVolume(): single;
begin
  Result := 0;
end;

procedure TSDLInputDevice.SetVolume(Volume: single);
begin
end;

function TAudioInput_SDL.GetName: String;
begin
  Result := 'SDL';
  if SDL_WasInit(SDL_INIT_AUDIO) <> 0 then
    Result := Result + ' (' + SDL_GetCurrentAudioDriver + ')';
end;

function TAudioInput_SDL.EnumDevices(): boolean;
var
  i, j:         integer;
  deviceIndex:  integer;
  maxDevices:   integer;
  name:         PChar;
  device:       TSDLInputDevice;
  spec:         TSDL_AudioSpec;
  dev:          TSDL_AudioDeviceID;
  deviceName:   UTF8String;
  portName:     UTF8String;
  port:         UTF8String;
  names:        TUTF8StringArray;
  ports:        TUTF8StringArray;
  hasTwin:      boolean;
  cfgIndex:     integer;
  cfgChanged:   boolean;
begin
  Result := false;

  Log.LogInfo('Using ' + SDL_GetCurrentAudioDriver + ' driver', 'SDL');

  cfgChanged := false;
  ListCaptureDevices(names, ports);
  maxDevices := Length(names);
  if maxDevices < 1 then
    maxDevices := 1;

  // init array-size to max. input-devices count
  SetLength(AudioInputProcessor.DeviceList, maxDevices);

  deviceIndex := 0;
  for i := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    name := nil;
    if i <= High(names) then
      name := PChar(names[i]);

    deviceName := DEFAULT_SOURCE_NAME;
    port := '';
    if (name <> nil) then
    begin
      deviceName := names[i];
      port := ports[i];

      if port <> '' then
      begin
        // Identical devices are named after the port they are plugged into.
        // SDL's own numbering depends on the order in which they were
        // connected, so players would get swapped every now and then.
        portName := StripDuplicateSuffix(names[i]) + ' [' + PortLabel(port) + ']';

        hasTwin := false;
        for j := 0 to High(names) do
          if (j <> i) and (StripDuplicateSuffix(names[j]) = StripDuplicateSuffix(names[i])) then
            hasTwin := true;

        // Once a device was configured by port keep that name,
        // even while its twin is not connected.
        if hasTwin or (FindDeviceConfig(portName) >= 0) then
        begin
          if FindDeviceConfig(portName) < 0 then
          begin
            // carry over the assignment made with SDL's name
            cfgIndex := FindDeviceConfig(names[i]);
            if cfgIndex >= 0 then
            begin
              Ini.InputDeviceConfig[cfgIndex].Name := portName;
              cfgChanged := true;
              Log.LogStatus('InputDevice "' + names[i] + '" is now configured as "' + portName + '"', 'SDL');
            end;
          end;
          deviceName := portName;
        end;
      end;
    end;
    deviceName := UnifyDeviceName(deviceName, i);
    if not ShouldProbeDevice(deviceName) then
      continue;

    FillChar(spec, SizeOf(spec), 0);
    with spec do
    begin
      freq := 44100;
      format := AUDIO_S16SYS;
      channels := 0; // override with SDL_AUDIO_CHANNELS
      samples := 0;
    end;

    dev := SDL_OpenAudioDevice(name, 1, @spec, @spec, SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or SDL_AUDIO_ALLOW_CHANNELS_CHANGE);
    if dev < 1 then
      continue;

    SDL_CloseAudioDevice(dev);

    device := TSDLInputDevice.Create();
    device.Name := deviceName;
    device.UseName := false;
    if name <> nil then
    begin
      device.UseName := true;
      device.SDLName := name;
      device.Port := port;
    end;

    device.MicSource := -1;
    device.SourceRestore := -1;
    SetLength(device.Source, 1);
    device.Source[0].Name := DEFAULT_SOURCE_NAME;

    // create audio-format info and resize capture-buffer array
    device.AudioFormat := TAudioFormatInfo.Create(
        spec.channels,
        spec.freq,
        asfS16
    );
    SetLength(device.CaptureChannel, device.AudioFormat.Channels);

    Log.LogStatus('InputDevice "' + device.Name + '"@' +
        IntToStr(device.AudioFormat.Channels) + 'x' +
        FloatToStr(device.AudioFormat.SampleRate) + 'Hz ' +
        'defaults to ' + IntToStr(spec.samples) + ' samples buffer',
        'SDL');

    AudioInputProcessor.DeviceList[deviceIndex] := device;
    Inc(deviceIndex);
  end;

  // keep the port based names, otherwise the assignment would be made
  // again on the next start, with whatever order the devices have then
  if cfgChanged then
    Ini.Save;

  // adjust size to actual input-device count
  SetLength(AudioInputProcessor.DeviceList, deviceIndex);
  Log.LogStatus('#Input-Devices: ' + IntToStr(deviceIndex), 'SDL');
  Result := (deviceIndex > 0) or (ScanMode = aimFast);
end;

procedure TAudioInput_SDL.CheckDevices;
var
  i:      integer;
  device: TAudioInputDevice;
begin
  if not Initialized then
    Exit;
  if int64(SDL_GetTicks) - int64(LastCheckTick) < DEVICE_CHECK_INTERVAL then
    Exit;
  LastCheckTick := SDL_GetTicks;

  for i := 0 to High(AudioInputProcessor.DeviceList) do
  begin
    device := AudioInputProcessor.DeviceList[i];
    if assigned(device) and (device is TSDLInputDevice) then
      TSDLInputDevice(device).CheckHealth();
  end;
end;

function TAudioInput_SDL.InitializeRecord(ScanMode: TAudioInputScanMode): boolean;
begin
  Result := false;

  if SDL_InitSubSystem(SDL_INIT_AUDIO) = -1 then
    Exit;

  Initialized := true;
  PrepareDeviceScan(ScanMode);
  Result := EnumDevices();
end;

function TAudioInput_SDL.FinalizeRecord: boolean;
begin
  CaptureStop;
  if Initialized then
  begin
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
    Initialized := false;
  end;
  Result := inherited FinalizeRecord();
end;

initialization
  MediaManager.add(TAudioInput_SDL.Create);

end.
