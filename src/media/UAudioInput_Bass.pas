{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

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
  UCommon,    // (Note: for MakeLong on non-windows platforms)
  {$IFDEF MSWINDOWS}
  Windows,    // (Note: for MakeLong)
  {$ENDIF}
  bass;       // (Note: DWORD is redefined here -> insert after Windows-unit)

type
  TAudioInput_Bass = class(TAudioInputBase)
    private
      function EnumDevices(): boolean;
    public
      function GetName: String; override;
      function InitializeRecord: boolean; override;
      function FinalizeRecord: boolean; override;
  end;

  TBassInputDevice = class(TAudioInputDevice)
    private
      RecordStream: HSTREAM;
      BassDeviceID: DWORD; // DeviceID used by BASS
      SingleIn: boolean;

      function SetInputSource(SourceIndex: integer): boolean;
      function GetInputSource(): integer;
    public
      function Open(): boolean;
      function Close(): boolean;
      function Start(): boolean; override;
      function Stop(): boolean;  override;

      function GetVolume(): single;        override;
      procedure SetVolume(Volume: single); override;
  end;

var
  BassCore: TAudioCore_Bass;


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
    len: integer; inputDevice: Pointer): boolean; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  AudioInputProcessor.HandleMicrophoneData(buffer, len, inputDevice);
  Result := true;
end;


{ TBassInputDevice }

function TBassInputDevice.GetInputSource(): integer;
var
  SourceCnt: integer;
  i: integer;
  flags: DWORD;
begin
  // get input-source config (subtract virtual device to get BASS indices)
  SourceCnt := Length(Source)-1;

  // find source
  Result := -1;
  for i := 0 to SourceCnt-1 do
  begin
    // get input settings
    flags := BASS_RecordGetInput(i, PSingle(nil)^);
    if (flags = DWORD(-1)) then
    begin
      Log.LogError('BASS_RecordGetInput: ' + BassCore.ErrorGetString(), 'TBassInputDevice.GetInputSource');
      Exit;
    end;

    // check if current source is selected
    if ((flags and BASS_INPUT_OFF) = 0) then
    begin
      // selected source found
      Result := i;
      Exit;
    end;
  end;
end;

function TBassInputDevice.SetInputSource(SourceIndex: integer): boolean;
var
  SourceCnt: integer;
  i: integer;
  flags: DWORD;
begin
  Result := false;

  // check for invalid source index
  if (SourceIndex < 0) then
    Exit;

  // get input-source config (subtract virtual device to get BASS indices)
  SourceCnt := Length(Source)-1;

  // turn on selected source (turns off the others for single-in devices)
  if (not BASS_RecordSetInput(SourceIndex, BASS_INPUT_ON, -1)) then
  begin
    Log.LogError('BASS_RecordSetInput: ' + BassCore.ErrorGetString(), 'TBassInputDevice.Start');
    Exit;
  end;

  // turn off all other sources (not needed for single-in devices)
  if (not SingleIn) then
  begin
    for i := 0 to SourceCnt-1 do
    begin
      if (i = SourceIndex) then
        continue;
      // get input settings
      flags := BASS_RecordGetInput(i, PSingle(nil)^);
      if (flags = DWORD(-1)) then
      begin
        Log.LogError('BASS_RecordGetInput: ' + BassCore.ErrorGetString(), 'TBassInputDevice.GetInputSource');
        Exit;
      end;
      // deselect source if selected
      if ((flags and BASS_INPUT_OFF) = 0) then
        BASS_RecordSetInput(i, BASS_INPUT_OFF, -1);
    end;
  end;

  Result := true;
end;

function TBassInputDevice.Open(): boolean;
var
  FormatFlags: DWORD;
  SourceIndex: integer;
const
  latency = 20; // 20ms callback period (= latency)
begin
  Result := false;

  if (not BASS_RecordInit(BassDeviceID)) then
  begin
    Log.LogError('BASS_RecordInit['+Name+']: ' +
                 BassCore.ErrorGetString(), 'TBassInputDevice.Open');
    Exit;
  end;

  if (not BassCore.ConvertAudioFormatToBASSFlags(AudioFormat.Format, FormatFlags)) then
  begin
    Log.LogError('Unhandled sample-format', 'TBassInputDevice.Open');
    Exit;
  end;

  // start capturing in paused state
  RecordStream := BASS_RecordStart(Round(AudioFormat.SampleRate), AudioFormat.Channels,
                    MakeLong(FormatFlags or BASS_RECORD_PAUSE, latency),
                    @MicrophoneCallback, Self);
  if (RecordStream = 0) then
  begin
    Log.LogError('BASS_RecordStart: ' + BassCore.ErrorGetString(), 'TBassInputDevice.Open');
    BASS_RecordFree;
    Exit;
  end;

  // save current source selection and select new source
  SourceIndex := Ini.InputDeviceConfig[CfgIndex].Input-1;
  if (SourceIndex = -1) then
  begin
    // nothing to do if default source is used
    SourceRestore := -1;
  end
  else
  begin
    // store current source-index and select new source
    SourceRestore := GetInputSource();
    SetInputSource(SourceIndex);
  end;

  Result := true;
end;

{* Start input-capturing on this device. *}
function TBassInputDevice.Start(): boolean;
begin
  Result := false;

  // recording already started -> stop first
  if (RecordStream <> 0) then
    Stop();

  // TODO: Do not open the device here (takes too much time).
  if not Open() then
    Exit;

  if (not BASS_ChannelPlay(RecordStream, true)) then
  begin
    Log.LogError('BASS_ChannelPlay: ' + BassCore.ErrorGetString(), 'TBassInputDevice.Start');
    Exit;
  end;

  Result := true;
end;

{* Stop input-capturing on this device. *}
function TBassInputDevice.Stop(): boolean;
begin
  Result := false;

  if (RecordStream = 0) then
    Exit;
  if (not BASS_RecordSetDevice(BassDeviceID)) then
    Exit;

  if (not BASS_ChannelStop(RecordStream)) then
  begin
    Log.LogError('BASS_ChannelStop: ' + BassCore.ErrorGetString(), 'TBassInputDevice.Stop');
  end;

  // TODO: Do not close the device here (takes too much time).
  Result := Close();
end;

function TBassInputDevice.Close(): boolean;
begin
  // restore source selection
  if (SourceRestore >= 0) then
  begin
    SetInputSource(SourceRestore);
  end;

  // free data
  if (not BASS_RecordFree()) then
  begin
    Log.LogError('BASS_RecordFree: ' + BassCore.ErrorGetString(), 'TBassInputDevice.Close');
    Result := false;
  end
  else
  begin
    Result := true;
  end;

  RecordStream := 0;
end;

function TBassInputDevice.GetVolume(): single;
var
  SourceIndex: integer;
  lVolume: Single;
begin
  Result := 0;

  SourceIndex := Ini.InputDeviceConfig[CfgIndex].Input-1;
  if (SourceIndex = -1) then
  begin
    // if default source used find selected source
    SourceIndex := GetInputSource();
    if (SourceIndex = -1) then
      Exit;
  end;

  if (BASS_RecordGetInput(SourceIndex, lVolume) = DWORD(-1)) then
  begin
    Log.LogError('BASS_RecordGetInput: ' + BassCore.ErrorGetString() , 'TBassInputDevice.GetVolume');
    Exit;
  end;
  Result := lVolume;
end;

procedure TBassInputDevice.SetVolume(Volume: single);
var
  SourceIndex: integer;
begin
  SourceIndex := Ini.InputDeviceConfig[CfgIndex].Input-1;
  if (SourceIndex = -1) then
  begin
    // if default source used find selected source
    SourceIndex := GetInputSource();
    if (SourceIndex = -1) then
      Exit;
  end;

  // clip volume to valid range
  if (Volume > 1.0) then
    Volume := 1.0
  else if (Volume < 0) then
    Volume := 0;

  if (not BASS_RecordSetInput(SourceIndex, 0, Volume)) then
  begin
    Log.LogError('BASS_RecordSetInput: ' + BassCore.ErrorGetString() , 'TBassInputDevice.SetVolume');
  end;
end;


{ TAudioInput_Bass }

function  TAudioInput_Bass.GetName: String;
begin
  result := 'BASS_Input';
end;

function TAudioInput_Bass.EnumDevices(): boolean;
var
  Descr:      PChar;
  SourceName: PChar;
  Flags:      integer;
  BassDeviceID: integer;
  BassDevice:   TBassInputDevice;
  DeviceIndex:  integer;
  DeviceInfo: BASS_DEVICEINFO;
  SourceIndex:  integer;
  RecordInfo: BASS_RECORDINFO;
  SelectedSourceIndex: integer;
begin
  result := false;

  DeviceIndex := 0;
  BassDeviceID := 0;
  SetLength(AudioInputProcessor.DeviceList, 0);

  // checks for recording devices and puts them into an array
  while true do
  begin
    if (not BASS_RecordGetDeviceInfo(BassDeviceID, DeviceInfo)) then
      break;

    // try to initialize the device
    if not BASS_RecordInit(BassDeviceID) then
    begin
      Log.LogStatus('Failed to initialize BASS Capture-Device['+inttostr(BassDeviceID)+']',
                    'TAudioInput_Bass.InitializeRecord');
    end
    else
    begin
      SetLength(AudioInputProcessor.DeviceList, DeviceIndex+1);

      // TODO: free object on termination
      BassDevice := TBassInputDevice.Create();
      AudioInputProcessor.DeviceList[DeviceIndex] := BassDevice;

      Descr := DeviceInfo.name;

      BassDevice.BassDeviceID := BassDeviceID;
      BassDevice.Name := UnifyDeviceName(Descr, DeviceIndex);

      // zero info-struct as some fields might not be set (e.g. freq is just set on Vista and MacOSX)
      FillChar(RecordInfo, SizeOf(RecordInfo), 0);
      // retrieve recording device info
      BASS_RecordGetInfo(RecordInfo);

      // check if BASS has capture-freq. info
      if (RecordInfo.freq > 0) then
      begin
        // use current input sample rate (available only on Windows Vista and OSX).
        // Recording at this rate will give the best quality and performance, as no resampling is required.
        // FIXME: does BASS use LSB/MSB or system integer values for 16bit?
        BassDevice.AudioFormat := TAudioFormatInfo.Create(2, RecordInfo.freq, asfS16)
      end
      else
      begin
        // BASS does not provide an explizit input channel count (except BASS_RECORDINFO.formats)
        // but it doesn't fail if we use stereo input on a mono device
        // -> use stereo by default
        BassDevice.AudioFormat := TAudioFormatInfo.Create(2, 44100, asfS16)
      end;

      // get info if multiple input-sources can be selected at once
      BassDevice.SingleIn := RecordInfo.singlein;

      // init list for capture buffers per channel
      SetLength(BassDevice.CaptureChannel, BassDevice.AudioFormat.Channels);

      BassDevice.MicSource := -1;
      BassDevice.SourceRestore := -1;

      // add a virtual default source (will not change mixer-settings)
      SetLength(BassDevice.Source, 1);
      BassDevice.Source[0].Name := DEFAULT_SOURCE_NAME;

      // add real input sources
      SourceIndex := 1;

      // process each input
      while true do
      begin
        SourceName := BASS_RecordGetInputName(SourceIndex-1);

        {$IFDEF DARWIN}
        // Under MacOSX the SingStar Mics have an empty InputName.
        // So, we have to add a hard coded Workaround for this problem
        // FIXME: - Do we need this anymore? Doesn't the (new) default source already solve this problem?
        //        - Normally a nil return value of BASS_RecordGetInputName() means end-of-list, so maybe
        //          BASS is not able to detect any mic-sources (the default source will work then).
        //        - Does BASS_RecordGetInfo() return true or false? If it returns true in this case
        //          we could use this value to check if the device exists.
        //          Please check that, eddie.
        //          If it returns false, then the source is not detected and it does not make sense to add a second
        //          fake device here.
        //          What about BASS_RecordGetInput()? Does it return a value <> -1?
        //        - Does it even work at all with this fake source-index, now that input switching works?
        //          This info was not used before (sources were never switched), so it did not matter what source-index was used.
        //          But now BASS_RecordSetInput() will probably fail.
        if ((SourceName = nil) and (SourceIndex = 1) and (Pos('USBMIC Serial#', Descr) > 0)) then
          SourceName := 'Microphone'
        {$ENDIF}
        
        if (SourceName = nil) then
          break;

        SetLength(BassDevice.Source, Length(BassDevice.Source)+1);
        BassDevice.Source[SourceIndex].Name := SourceName;

        // get input-source info
        Flags := BASS_RecordGetInput(SourceIndex, PSingle(nil)^);
        if (Flags <> -1) then
        begin
          // is the current source a mic-source?
          if ((Flags and BASS_INPUT_TYPE_MIC) <> 0) then
            BassDevice.MicSource := SourceIndex;
        end;

        Inc(SourceIndex);
      end;

      // FIXME: this call hangs in FPC (windows) every 2nd time USDX is called.
      //   Maybe because the sound-device was not released properly?
      BASS_RecordFree;

      Inc(DeviceIndex);
    end;

    Inc(BassDeviceID);
  end;

  result := true;
end;

function TAudioInput_Bass.InitializeRecord(): boolean;
begin
  BassCore := TAudioCore_Bass.GetInstance();
  if not BassCore.CheckVersion then
  begin
    Result := false;
    Exit;
  end;
  Result := EnumDevices();
end;

function TAudioInput_Bass.FinalizeRecord(): boolean;
begin
  CaptureStop;
  Result := inherited FinalizeRecord;
end;


initialization
  MediaManager.Add(TAudioInput_Bass.Create);

end.
