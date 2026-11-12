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
 *}

unit UPulseSources;

{*
 * Asks PulseAudio (or PipeWire's PulseAudio server) for its recording
 * devices and where they are plugged in.
 *
 * SDL only tells us a description like "USB Microphone". Two microphones of
 * the same model get the same description and can only be told apart by the
 * port they are connected to, which SDL does not expose.
 *
 * libpulse is loaded at runtime, so there is no build dependency. If it is
 * missing or no server answers, GetPulseSources just returns false.
 *}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}

type
  TPulseSource = record
    Name:        UTF8String; // unique PulseAudio name of the source
    Description: UTF8String; // human readable name, this is what SDL shows
    BusPath:     UTF8String; // physical connection, e.g. pci-0000:09:00.0-usb-0:3:1.0
  end;
  TPulseSourceArray = array of TPulseSource;

{**
 * Lists the recording devices in the order the sound server reports them.
 * Monitors of playback devices are left out (SDL does not list them either).
 * Returns false if the sound server could not be asked.
 *}
function GetPulseSources(out Sources: TPulseSourceArray): boolean;

implementation

{$IFDEF UNIX}

uses
  SysUtils,
  ctypes,
  dynlibs,
  sdl2;

const
  PULSE_LIB_NAME = 'libpulse.so.0';
  PULSE_TIMEOUT  = 500; // ms we are willing to wait for the server

  PA_CONTEXT_NOAUTOSPAWN = 1;
  PA_CONTEXT_READY       = 4;
  PA_CONTEXT_FAILED      = 5;
  PA_CONTEXT_TERMINATED  = 6;
  PA_OPERATION_RUNNING   = 0;
  PA_INVALID_INDEX       = $FFFFFFFF;

{$PACKRECORDS C}
type
  // beginning of pa_source_info from pulse/introspect.h
  PPaSourceInfo = ^TPaSourceInfo;
  TPaSourceInfo = record
    name:        PChar;
    index:       cuint32;
    description: PChar;
    sample_spec: record
      format:   cint;
      rate:     cuint32;
      channels: cuint8;
    end;
    channel_map: record
      channels: cuint8;
      map:      array[0..31] of cint;
    end;
    owner_module: cuint32;
    volume: record
      channels: cuint8;
      values:   array[0..31] of cuint32;
    end;
    mute:                 cint;
    monitor_of_sink:      cuint32;
    monitor_of_sink_name: PChar;
    latency:              cuint64;
    driver:               PChar;
    flags:                cint;
    proplist:             pointer;
  end;
{$PACKRECORDS DEFAULT}

  TPaSourceInfoCallback = procedure(context: pointer; info: PPaSourceInfo;
                                    eol: cint; userdata: pointer); cdecl;

var
  LibHandle: TLibHandle = NilHandle;
  LibTried:  boolean = false;

  pa_mainloop_new:       function(): pointer; cdecl;
  pa_mainloop_get_api:   function(mainloop: pointer): pointer; cdecl;
  pa_mainloop_iterate:   function(mainloop: pointer; block: cint; retval: pcint): cint; cdecl;
  pa_mainloop_free:      procedure(mainloop: pointer); cdecl;
  pa_context_new:        function(api: pointer; name: PChar): pointer; cdecl;
  pa_context_connect:    function(context: pointer; server: PChar; flags: cint; spawnApi: pointer): cint; cdecl;
  pa_context_get_state:  function(context: pointer): cint; cdecl;
  pa_context_disconnect: procedure(context: pointer); cdecl;
  pa_context_unref:      procedure(context: pointer); cdecl;
  pa_context_get_source_info_list: function(context: pointer; cb: TPaSourceInfoCallback; userdata: pointer): pointer; cdecl;
  pa_operation_get_state: function(operation: pointer): cint; cdecl;
  pa_operation_unref:    procedure(operation: pointer); cdecl;
  pa_proplist_gets:      function(proplist: pointer; key: PChar): PChar; cdecl;

function LoadPulse(): boolean;

  function Get(const name: string): pointer;
  begin
    Result := GetProcedureAddress(LibHandle, name);
    if Result = nil then
      LoadPulse := false;
  end;

begin
  if LibTried then
  begin
    Result := (LibHandle <> NilHandle);
    Exit;
  end;
  LibTried := true;
  Result := false;

  LibHandle := LoadLibrary(PULSE_LIB_NAME);
  if LibHandle = NilHandle then
    Exit;

  Result := true;
  @pa_mainloop_new       := Get('pa_mainloop_new');
  @pa_mainloop_get_api   := Get('pa_mainloop_get_api');
  @pa_mainloop_iterate   := Get('pa_mainloop_iterate');
  @pa_mainloop_free      := Get('pa_mainloop_free');
  @pa_context_new        := Get('pa_context_new');
  @pa_context_connect    := Get('pa_context_connect');
  @pa_context_get_state  := Get('pa_context_get_state');
  @pa_context_disconnect := Get('pa_context_disconnect');
  @pa_context_unref      := Get('pa_context_unref');
  @pa_context_get_source_info_list := Get('pa_context_get_source_info_list');
  @pa_operation_get_state := Get('pa_operation_get_state');
  @pa_operation_unref    := Get('pa_operation_unref');
  @pa_proplist_gets      := Get('pa_proplist_gets');

  if not Result then
  begin
    UnloadLibrary(LibHandle);
    LibHandle := NilHandle;
  end;
end;

type
  PPulseSourceArray = ^TPulseSourceArray;

procedure SourceInfoCallback(context: pointer; info: PPaSourceInfo;
                             eol: cint; userdata: pointer); cdecl;
var
  List:    PPulseSourceArray;
  BusPath: PChar;
begin
  if (eol <> 0) or (info = nil) then
    Exit;
  if info^.monitor_of_sink <> PA_INVALID_INDEX then
    Exit;

  List := PPulseSourceArray(userdata);
  SetLength(List^, Length(List^) + 1);
  with List^[High(List^)] do
  begin
    Name := info^.name;
    Description := info^.description;
    BusPath := '';
  end;

  if info^.proplist <> nil then
  begin
    // PipeWire uses an underscore, PulseAudio a dot
    BusPath := pa_proplist_gets(info^.proplist, 'device.bus_path');
    if BusPath = nil then
      BusPath := pa_proplist_gets(info^.proplist, 'device.bus-path');
    if BusPath <> nil then
      List^[High(List^)].BusPath := BusPath;
  end;
end;

function GetPulseSources(out Sources: TPulseSourceArray): boolean;
var
  Mainloop:  pointer;
  Context:   pointer;
  Operation: pointer;
  Deadline:  int64;
  State:     cint;
begin
  Result := false;
  SetLength(Sources, 0);

  if not LoadPulse() then
    Exit;

  Mainloop := pa_mainloop_new();
  if Mainloop = nil then
    Exit;

  Context := pa_context_new(pa_mainloop_get_api(Mainloop), 'UltraStar Deluxe');
  if Context <> nil then
  begin
    Deadline := int64(SDL_GetTicks) + PULSE_TIMEOUT;

    if pa_context_connect(Context, nil, PA_CONTEXT_NOAUTOSPAWN, nil) >= 0 then
    begin
      // wait for the connection without ever blocking for long
      repeat
        pa_mainloop_iterate(Mainloop, 0, nil);
        State := pa_context_get_state(Context);
        if State < PA_CONTEXT_READY then
          SDL_Delay(1);
      until (State >= PA_CONTEXT_READY) or (int64(SDL_GetTicks) > Deadline);

      if State = PA_CONTEXT_READY then
      begin
        Operation := pa_context_get_source_info_list(Context, @SourceInfoCallback, @Sources);
        if Operation <> nil then
        begin
          while (pa_operation_get_state(Operation) = PA_OPERATION_RUNNING) and
                (int64(SDL_GetTicks) <= Deadline) do
          begin
            if pa_mainloop_iterate(Mainloop, 0, nil) = 0 then
              SDL_Delay(1);
          end;
          Result := (pa_operation_get_state(Operation) <> PA_OPERATION_RUNNING);
          pa_operation_unref(Operation);
        end;
      end;

      pa_context_disconnect(Context);
    end;
    pa_context_unref(Context);
  end;

  pa_mainloop_free(Mainloop);

  if not Result then
    SetLength(Sources, 0);
end;

{$ELSE}

function GetPulseSources(out Sources: TPulseSourceArray): boolean;
begin
  SetLength(Sources, 0);
  Result := false;
end;

{$ENDIF}

end.
