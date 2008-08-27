unit UMediaCore_SDL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  sdl;

function ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;

implementation

function ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;
begin
  case Format of
    asfU8:     SDLFormat := AUDIO_U8;
    asfS8:     SDLFormat := AUDIO_S8;
    asfU16LSB: SDLFormat := AUDIO_U16LSB;
    asfS16LSB: SDLFormat := AUDIO_S16LSB;
    asfU16MSB: SDLFormat := AUDIO_U16MSB;
    asfS16MSB: SDLFormat := AUDIO_S16MSB;
    asfU16:    SDLFormat := AUDIO_U16;
    asfS16:    SDLFormat := AUDIO_S16;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

end.
