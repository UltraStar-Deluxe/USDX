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

unit UMedia_dummy;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
  SysUtils,
  math,
  UTime,
  UMusic,
  UPath;

type
    TAudio_Dummy = class( TInterfacedObject, IAudioPlayback, IAudioInput )
    private
      DummyOutputDeviceList: TAudioOutputDeviceList;
    public
      constructor Create();
      function GetName: string;

      function Init(): boolean;
      function Finalize(): boolean;

      function Open(const aFileName: IPath): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function  GetPosition: real;

      procedure SetSyncSource(SyncSource: TSyncSource);

      // IAudioInput
      function InitializeRecord: boolean;
      function FinalizeRecord: boolean;
      procedure CaptureStart;
      procedure CaptureStop;
      procedure GetFFTData(var data: TFFTData);
      function GetPCMData(var data: TPCMData): Cardinal;

      // IAudioPlayback
      function InitializePlayback: boolean;
      function FinalizePlayback: boolean;

      function GetOutputDeviceList(): TAudioOutputDeviceList;
      procedure FadeIn(Time: real; TargetVolume: single);
      procedure SetAppVolume(Volume: single);
      procedure SetVolume(Volume: single);
      procedure Rewind;

      procedure SetLoop(Enabled: boolean);
      function GetLoop(): boolean;

      function Finished: boolean;
      function Length: real;

      function OpenSound(const Filename: IPath): TAudioPlaybackStream;
      procedure CloseSound(var PlaybackStream: TAudioPlaybackStream);
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);

      function CreateVoiceStream(Channel: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream;
      procedure CloseVoiceStream(var VoiceStream: TAudioVoiceStream);
    end;

    TVideo_Dummy = class( TInterfacedObject, IVideo )
    public
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetLoop(Enable: boolean);
      function GetLoop(): boolean;

      procedure SetPosition(Time: real);
      function GetPosition: real;

      procedure GetFrame(Time: Extended);
      procedure DrawGL(Screen: integer);

      property Loop: boolean read GetLoop write SetLoop;
      property Position: real read GetPosition write SetPosition;
    end;

    TVideoPlayback_Dummy = class( TInterfacedObject, IVideoPlayback, IVideoVisualization )
    public
      constructor Create();
      function GetName: string;

      function Init(): boolean;
      function Finalize(): boolean;

      function Open(const FileName: IPath): IVideo;
    end;

function  TAudio_Dummy.GetName: string;
begin
  Result := 'AudioDummy';
end;

constructor TAudio_Dummy.Create();
begin
  inherited;
end;

function TAudio_Dummy.Init(): boolean;
begin
  Result := true;
end;

function TAudio_Dummy.Finalize(): boolean;
begin
  Result := true;
end;

function TAudio_Dummy.Open(const aFileName : IPath): boolean; // true if succeed
begin
  Result := false;
end;

procedure TAudio_Dummy.Close;
begin
end;

procedure TAudio_Dummy.Play;
begin
end;

procedure TAudio_Dummy.Pause;
begin
end;

procedure TAudio_Dummy.Stop;
begin
end;

procedure TAudio_Dummy.SetPosition(Time: real);
begin
end;

function  TAudio_Dummy.GetPosition: real;
begin
  Result := 0;
end;

procedure TAudio_Dummy.SetSyncSource(SyncSource: TSyncSource);
begin
end;

// IAudioInput
function TAudio_Dummy.InitializeRecord: boolean;
begin
  Result := true;
end;

function TAudio_Dummy.FinalizeRecord: boolean;
begin
  Result := true;
end;

procedure TAudio_Dummy.CaptureStart;
begin
end;

procedure TAudio_Dummy.CaptureStop;
begin
end;

procedure TAudio_Dummy.GetFFTData(var data: TFFTData);
begin
end;

function  TAudio_Dummy.GetPCMData(var data: TPCMData): Cardinal;
begin
  Result := 0;
end;

// IAudioPlayback
function TAudio_Dummy.InitializePlayback: boolean;
begin
  SetLength(DummyOutputDeviceList, 1);
  DummyOutputDeviceList[0] := TAudioOutputDevice.Create();
  DummyOutputDeviceList[0].Name := '[Dummy Device]';
  Result := true;
end;

function TAudio_Dummy.FinalizePlayback: boolean;
begin
  Result := true;
end;

function TAudio_Dummy.GetOutputDeviceList(): TAudioOutputDeviceList;
begin
  Result := DummyOutputDeviceList;
end;

procedure TAudio_Dummy.SetAppVolume(Volume: single);
begin
end;

procedure TAudio_Dummy.SetVolume(Volume: single);
begin
end;

procedure TAudio_Dummy.SetLoop(Enabled: boolean);
begin
end;

function TAudio_Dummy.GetLoop(): boolean;
begin
  Result := false;
end;

procedure TAudio_Dummy.FadeIn(Time: real; TargetVolume: single);
begin
end;

procedure TAudio_Dummy.Rewind;
begin
end;

function TAudio_Dummy.Finished: boolean;
begin
  Result := false;
end;

function TAudio_Dummy.Length: real;
begin
  Result := 60;
end;

function TAudio_Dummy.OpenSound(const Filename: IPath): TAudioPlaybackStream;
begin
 Result := nil;
end;

procedure TAudio_Dummy.CloseSound(var PlaybackStream: TAudioPlaybackStream);
begin
end;

procedure TAudio_Dummy.PlaySound(stream: TAudioPlaybackStream);
begin
end;

procedure TAudio_Dummy.StopSound(stream: TAudioPlaybackStream);
begin
end;

function TAudio_Dummy.CreateVoiceStream(Channel: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream;
begin
  Result := nil;
end;

procedure TAudio_Dummy.CloseVoiceStream(var VoiceStream: TAudioVoiceStream);
begin
end;


{ TVideoPlayback_Dummy }

procedure TVideo_Dummy.Close;
begin
end;

procedure TVideo_Dummy.Play;
begin
end;

procedure TVideo_Dummy.Pause;
begin
end;

procedure TVideo_Dummy.Stop;
begin
end;

procedure TVideo_Dummy.SetLoop(Enable: boolean);
begin
end;

function TVideo_Dummy.GetLoop(): boolean;
begin
  Result := false;
end;

procedure TVideo_Dummy.SetPosition(Time: real);
begin
end;

function TVideo_Dummy.GetPosition: real;
begin
  Result := 0;
end;

procedure TVideo_Dummy.GetFrame(Time: Extended);
begin
end;

procedure TVideo_Dummy.DrawGL(Screen: integer);
begin
end;


{ TVideoPlayback_Dummy }

constructor TVideoPlayback_Dummy.Create();
begin
end;

function TVideoPlayback_Dummy.GetName: string;
begin
  Result := 'VideoDummy';
end;

function TVideoPlayback_Dummy.Init(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_Dummy.Finalize(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_Dummy.Open(const FileName: IPath): IVideo;
begin
  Result := TVideo_Dummy.Create;
end;


initialization
  MediaManager.Add(TAudio_Dummy.Create);
  MediaManager.Add(TVideoPlayback_Dummy.Create);

end.
