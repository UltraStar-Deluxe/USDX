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

unit UAudioDecoder_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
  Classes,
  SysUtils,
  UMain,
  UMusic,
  UAudioCore_Bass,
  ULog,
  bass;

type
  TBassDecodeStream = class(TAudioDecodeStream)
    private
      Handle: HSTREAM;
      FormatInfo : TAudioFormatInfo;
      Error: boolean;
    public
      constructor Create(Handle: HSTREAM);
      destructor Destroy(); override;

      procedure Close();                     override;

      function GetLength(): real;            override;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;
      function GetPosition: real;            override;
      procedure SetPosition(Time: real);     override;
      function GetLoop(): boolean;           override;
      procedure SetLoop(Enabled: boolean);   override;
      function IsEOF(): boolean;             override;
      function IsError(): boolean;           override;

      function ReadData(Buffer: PChar; BufSize: integer): integer; override;
  end;

type
  TAudioDecoder_Bass = class( TInterfacedObject, IAudioDecoder )
    public
      function GetName: string;

      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      function Open(const Filename: string): TAudioDecodeStream;
  end;

var
  BassCore: TAudioCore_Bass;


{ TBassDecodeStream }

constructor TBassDecodeStream.Create(Handle: HSTREAM);
var
  ChannelInfo: BASS_CHANNELINFO;
  Format: TAudioSampleFormat;
begin
  inherited Create();
  Self.Handle := Handle;

  // setup format info
  if (not BASS_ChannelGetInfo(Handle, ChannelInfo)) then
  begin
    raise Exception.Create('Failed to open decode-stream');
  end;
  BassCore.ConvertBASSFlagsToAudioFormat(ChannelInfo.flags, Format);
  FormatInfo := TAudioFormatInfo.Create(ChannelInfo.chans, ChannelInfo.freq, format);

  Error := false;
end;

destructor TBassDecodeStream.Destroy();
begin
  Close();
  inherited;
end;

procedure TBassDecodeStream.Close();
begin
  if (Handle <> 0) then
  begin
    BASS_StreamFree(Handle);
    Handle := 0;
  end;
  PerformOnClose();
  FreeAndNil(FormatInfo);
  Error := false;
end;

function TBassDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FormatInfo;
end;

function TBassDecodeStream.GetLength(): real;
var
  bytes: QWORD;
begin
  bytes  := BASS_ChannelGetLength(Handle, BASS_POS_BYTE);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

function TBassDecodeStream.GetPosition: real;
var
  bytes: QWORD;
begin
  bytes  := BASS_ChannelGetPosition(Handle, BASS_POS_BYTE);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

procedure TBassDecodeStream.SetPosition(Time: real);
var
  bytes: QWORD;
begin
  bytes := BASS_ChannelSeconds2Bytes(Handle, Time);
  BASS_ChannelSetPosition(Handle, bytes, BASS_POS_BYTE);
end;

function TBassDecodeStream.GetLoop(): boolean;
var
  flags: DWORD;
begin
  // retrieve channel flags
  flags := BASS_ChannelFlags(Handle, 0, 0);
  if (flags = DWORD(-1)) then
  begin
    Log.LogError('BASS_ChannelFlags: ' + BassCore.ErrorGetString(), 'TBassDecodeStream.GetLoop');
    Result := false;
    Exit;
  end;
  Result := (flags and BASS_SAMPLE_LOOP) <> 0;
end;

procedure TBassDecodeStream.SetLoop(Enabled: boolean);
var
  flags: DWORD;
begin
  // set/unset loop-flag
  if (Enabled) then
    flags := BASS_SAMPLE_LOOP
  else
    flags := 0;

  // set new flag-bits
  if (BASS_ChannelFlags(Handle, flags, BASS_SAMPLE_LOOP) = DWORD(-1)) then
  begin
    Log.LogError('BASS_ChannelFlags: ' + BassCore.ErrorGetString(), 'TBassDecodeStream.SetLoop');
    Exit;
  end;
end;

function TBassDecodeStream.IsEOF(): boolean;
begin
  Result := (BASS_ChannelIsActive(Handle) = BASS_ACTIVE_STOPPED);
end;

function TBassDecodeStream.IsError(): boolean;
begin
  Result := Error;
end;

function TBassDecodeStream.ReadData(Buffer: PChar; BufSize: integer): integer;
begin
  Result := BASS_ChannelGetData(Handle, Buffer, BufSize);
  // check error state (do not handle EOF as error)
  if ((Result = -1) and (BASS_ErrorGetCode() <> BASS_ERROR_ENDED)) then
    Error := true
  else
    Error := false;
end;


{ TAudioDecoder_Bass }

function TAudioDecoder_Bass.GetName: String;
begin
  result := 'BASS_Decoder';
end;

function TAudioDecoder_Bass.InitializeDecoder(): boolean;
begin
  BassCore := TAudioCore_Bass.GetInstance();
  Result := true;
end;

function TAudioDecoder_Bass.FinalizeDecoder(): boolean;
begin
  Result := true;
end;

function TAudioDecoder_Bass.Open(const Filename: string): TAudioDecodeStream;
var
  Stream: HSTREAM;
  ChannelInfo: BASS_CHANNELINFO;
  FileExt: string;
begin
  Result := nil;

  // check if BASS was initialized
  // in case the decoder is not used with BASS playback, init the NO_SOUND device
  if ((integer(BASS_GetDevice) = -1) and (BASS_ErrorGetCode() = BASS_ERROR_INIT)) then
    BASS_Init(0, 44100, 0, 0, nil);

  // TODO: use BASS_STREAM_PRESCAN for accurate seeking in VBR-files?
  //       disadvantage: seeking will slow down.
  Stream := BASS_StreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE);
  if (Stream = 0) then
  begin
    //Log.LogError(BassCore.ErrorGetString(), 'TAudioDecoder_Bass.Open');
    Exit;
  end;

  // check if BASS opened some erroneously recognized file-formats
  if BASS_ChannelGetInfo(Stream, channelInfo) then
  begin
    fileExt := ExtractFileExt(Filename);
    // BASS opens FLV-files (maybe others too) although it cannot handle them.
    // Setting BASS_CONFIG_VERIFY to the max. value (100000) does not help.
    if ((fileExt = '.flv') and (channelInfo.ctype = BASS_CTYPE_STREAM_MP1)) then
    begin
      BASS_StreamFree(Stream);
      Exit;
    end;
  end;

  Result := TBassDecodeStream.Create(Stream);
end;


initialization
  MediaManager.Add(TAudioDecoder_Bass.Create);

end.
