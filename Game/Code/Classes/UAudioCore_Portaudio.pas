unit UAudioCore_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}


uses
  Classes,
  SysUtils,
  portaudio;

type
  TAudioCore_Portaudio = class
    public
      class function GetPreferredApiIndex(): TPaHostApiIndex;
  end;

implementation

uses
  ULog;

{*
 * The default API used by Portaudio is the least common denominator
 * and might lack efficiency. In addition it might not even work.
 * We use an array named ApiPreferenceOrder with which we define the order of
 * preferred APIs to use. The first API-type in the list is tried first.
 * If it is not available the next one is tried and so on ...
 * If none of the preferred APIs was found the default API (detected by
 * portaudio) is used.
 *
 * Pascal does not permit zero-length static arrays, so you must use paDefaultApi
 * as an array's only member if you do not have any preferences.
 * You can also append paDefaultApi to a non-zero length preferences array but
 * this is optional because the default API is always used as a fallback.
 *}
const
  paDefaultApi = -1;
const
  ApiPreferenceOrder:
{$IF Defined(MSWINDOWS)}
    // Note1: Portmixer has no mixer support for paASIO and paWASAPI at the moment
    // Note2: Windows Default-API is MME, but DirectSound is faster
    array[0..0] of TPaHostApiTypeId = ( paDirectSound );
{$ELSEIF Defined(LINUX)}
    // Note: Portmixer has no mixer support for JACK at the moment
    array[0..2] of TPaHostApiTypeId = ( paALSA, paJACK, paOSS );
{$ELSEIF Defined(DARWIN)}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi ); // paCoreAudio
{$ELSE}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$IFEND}


{ TAudioInput_Portaudio }

class function TAudioCore_Portaudio.GetPreferredApiIndex(): TPaHostApiIndex;
var
  i: integer;
  apiIndex: TPaHostApiIndex;
  apiInfo:  PPaHostApiInfo;
begin
  result := -1;

  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
    if(ApiPreferenceOrder[i] <> paDefaultApi) then
    begin
      // check if API is available
      apiIndex := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if(apiIndex >= 0) then
      begin
        // we found an API but we must check if it works
        // (on linux portaudio might detect OSS but does not provide
        // any devices if ALSA is enabled)
        apiInfo := Pa_GetHostApiInfo(apiIndex);
        if (apiInfo^.deviceCount > 0) then
        begin
          Result := apiIndex;
          break;
        end;
      end;
    end;
  end;

  // None of the preferred APIs is available -> use default
  if(result < 0) then
  begin
    result := Pa_GetDefaultHostApi();
  end;
end;

end.
