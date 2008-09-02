unit UAudioCore_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  SysUtils,
  UMusic,
  bass;     // (Note: DWORD is defined here)

type
  TAudioCore_Bass = class
    public
      constructor Create();
      class function GetInstance(): TAudioCore_Bass;
      function ErrorGetString(): string; overload;
      function ErrorGetString(errCode: integer): string; overload;
      function ConvertAudioFormatToBASSFlags(Format: TAudioSampleFormat; out Flags: DWORD): boolean;
      function ConvertBASSFlagsToAudioFormat(Flags: DWORD; out Format: TAudioSampleFormat): boolean;
  end;

implementation

uses
  UMain,
  ULog;

var
  Instance: TAudioCore_Bass;

constructor TAudioCore_Bass.Create();
begin
  inherited;
end;

class function TAudioCore_Bass.GetInstance(): TAudioCore_Bass;
begin
  if (not Assigned(Instance)) then
    Instance := TAudioCore_Bass.Create();
  Result := Instance;
end;

function TAudioCore_Bass.ErrorGetString(): string;
begin
  Result := ErrorGetString(BASS_ErrorGetCode());
end;

function TAudioCore_Bass.ErrorGetString(errCode: integer): string;
begin
  case errCode of
    BASS_OK:             result := 'No error';
    BASS_ERROR_MEM:      result := 'Insufficient memory';
    BASS_ERROR_FILEOPEN: result := 'File could not be opened';
    BASS_ERROR_DRIVER:   result := 'Device driver not available';
    BASS_ERROR_BUFLOST:  result := 'Buffer lost';
    BASS_ERROR_HANDLE:   result := 'Invalid Handle';
    BASS_ERROR_FORMAT:   result := 'Sample-Format not supported';
    BASS_ERROR_POSITION: result := 'Illegal position';
    BASS_ERROR_INIT:     result := 'BASS_Init has not been successfully called';
    BASS_ERROR_START:    result := 'Paused/stopped';
    BASS_ERROR_ALREADY:  result := 'Already created/used';
    BASS_ERROR_NOCHAN:   result := 'No free channels';
    BASS_ERROR_ILLTYPE:  result := 'Type is invalid';
    BASS_ERROR_ILLPARAM: result := 'Illegal parameter';
    BASS_ERROR_NO3D:     result := 'No 3D support';
    BASS_ERROR_NOEAX:    result := 'No EAX support';
    BASS_ERROR_DEVICE:   result := 'Invalid device number';
    BASS_ERROR_NOPLAY:   result := 'Channel not playing';
    BASS_ERROR_FREQ:     result := 'Freq out of range';
    BASS_ERROR_NOTFILE:  result := 'Not a file stream';
    BASS_ERROR_NOHW:     result := 'No hardware support';
    BASS_ERROR_EMPTY:    result := 'Is empty';
    BASS_ERROR_NONET:    result := 'Network unavailable';
    BASS_ERROR_CREATE:   result := 'Creation error';
    BASS_ERROR_NOFX:     result := 'DX8 effects unavailable';
    BASS_ERROR_NOTAVAIL: result := 'Not available';
    BASS_ERROR_DECODE:   result := 'Is a decoding channel';
    BASS_ERROR_DX:       result := 'Insufficient version of DirectX';
    BASS_ERROR_TIMEOUT:  result := 'Timeout';
    BASS_ERROR_FILEFORM: result := 'File-Format not recognised/supported';
    BASS_ERROR_SPEAKER:  result := 'Requested speaker(s) not support';
    BASS_ERROR_VERSION:  result := 'Version error';
    BASS_ERROR_CODEC:    result := 'Codec not available/supported';
    BASS_ERROR_ENDED:    result := 'The channel/file has ended';
    BASS_ERROR_UNKNOWN:  result := 'Unknown error';
    else                 result := 'Unknown error';
  end;
end;

function TAudioCore_Bass.ConvertAudioFormatToBASSFlags(Format: TAudioSampleFormat; out Flags: DWORD): boolean;
begin
  case Format of
    asfS16:   Flags := 0;
    asfFloat: Flags := BASS_SAMPLE_FLOAT;
    asfU8:    Flags := BASS_SAMPLE_8BITS;
    else begin
      Result := false;
      Exit;
    end;
  end;

  Result := true;
end;

function TAudioCore_Bass.ConvertBASSFlagsToAudioFormat(Flags: DWORD; out Format: TAudioSampleFormat): boolean;
begin
  if ((Flags and BASS_SAMPLE_FLOAT) <> 0) then
    Format := asfFloat
  else if ((Flags and BASS_SAMPLE_8BITS) <> 0) then
    Format := asfU8
  else
    Format := asfS16;

  Result := true;
end;

end.
