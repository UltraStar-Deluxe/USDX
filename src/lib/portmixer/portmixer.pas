{*
 * PortMixer
 * PortMixer API Header File
 *
 * Copyright (c) 2002, 2006
 *
 * Written by Dominic Mazzoni
 *        and Leland Lucius
 *
 * PortMixer is intended to work side-by-side with PortAudio,
 * the Portable Real-Time Audio Library by Ross Bencina and
 * Phil Burk.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *}
unit portmixer;

{$IFDEF FPC}
  {$PACKRECORDS C}    (* GCC/Visual C/C++ compatible record packing *)
  {$MODE DELPHI }
{$ENDIF}

interface

uses
  ctypes,
  portaudio;

const
{$IF Defined(MSWINDOWS)}
  LibName = 'portmixer.dll';
{$ELSEIF Defined(DARWIN)}
//  LibName = 'libportmixer.dylib';
//  {$LINKLIB libportaudio}
{$ELSEIF Defined(UNIX)}
  LibName = 'libportmixer.so';
{$IFEND}

type
  PPxMixer = Pointer;
  TPxVolume = cfloat; {* 0.0 (min) --> 1.0 (max) *}
  TPxBalance = cfloat; {* -1.0 (left) --> 1.0 (right) *}

{*
 Px_OpenMixer() returns a mixer which will work with the given PortAudio
 audio device.  Pass 0 as the index for the first (default) mixer.
*}

function Px_OpenMixer( pa_stream: Pointer; i: cint ): PPxMixer; cdecl; external LibName;

{*
 Px_CloseMixer() closes a mixer opened using Px_OpenMixer and frees any
 memory associated with it. 
*}

procedure Px_CloseMixer( mixer: PPxMixer ); cdecl; external LibName;

{*
 Px_GetNumMixers returns the number of mixers which could be
 used with the given PortAudio device.  On most systems, there
 will be only one mixer for each device; however there may be
 multiple mixers for each device, or possibly multiple mixers
 which are independent of any particular PortAudio device.
*}

function Px_GetNumMixers( mixer: PPxMixer ): cint; cdecl; external LibName;
function Px_GetMixerName( mixer: PPxMixer; i: cint ): PChar; cdecl; external LibName;

{*
 Master (output) volume
*}

function Px_GetMasterVolume( mixer: PPxMixer ): TPxVolume; cdecl; external LibName;
procedure Px_SetMasterVolume( mixer: PPxMixer; volume: TPxVolume ); cdecl; external LibName;

{*
 Main output volume
*}

function Px_GetPCMOutputVolume( mixer: PPxMixer ): TPxVolume; cdecl; external LibName;
procedure Px_SetPCMOutputVolume( mixer: PPxMixer; volume: TPxVolume ); cdecl; external LibName;
function Px_SupportsPCMOutputVolume( mixer: PPxMixer ): cint; cdecl; external LibName;

{*
 All output volumes
*}

function Px_GetNumOutputVolumes( mixer: PPxMixer ): cint; cdecl; external LibName;
function Px_GetOutputVolumeName( mixer: PPxMixer; i: cint ): PChar; cdecl; external LibName;
function Px_GetOutputVolume( mixer: PPxMixer; i: cint ): TPxVolume; cdecl; external LibName;
procedure Px_SetOutputVolume( mixer: PPxMixer; i: cint; volume: TPxVolume ); cdecl; external LibName;

{*
 Input source
*}

function Px_GetNumInputSources( mixer: PPxMixer ): cint; cdecl; external LibName;
function Px_GetInputSourceName( mixer: PPxMixer; i: cint): PChar; cdecl; external LibName;
function Px_GetCurrentInputSource( mixer: PPxMixer ): cint; cdecl; external LibName; {* may return -1 == none *}
procedure Px_SetCurrentInputSource( mixer: PPxMixer; i: cint ); cdecl; external LibName;

{*
 Input volume
*}

function Px_GetInputVolume( mixer: PPxMixer ): TPxVolume; cdecl; external LibName;
procedure Px_SetInputVolume( mixer: PPxMixer; volume: TPxVolume ); cdecl; external LibName;

{*
  Balance
*}

function Px_SupportsOutputBalance( mixer: PPxMixer ): cint; cdecl; external LibName;
function Px_GetOutputBalance( mixer: PPxMixer ): TPxBalance; cdecl; external LibName;
procedure Px_SetOutputBalance( mixer: PPxMixer; balance: TPxBalance ); cdecl; external LibName;

{*
  Playthrough
*}

function Px_SupportsPlaythrough( mixer: PPxMixer ): cint; cdecl; external LibName;
function Px_GetPlaythrough( mixer: PPxMixer ): TPxVolume; cdecl; external LibName;
procedure Px_SetPlaythrough( mixer: PPxMixer; volume: TPxVolume ); cdecl; external LibName;

implementation

end.
