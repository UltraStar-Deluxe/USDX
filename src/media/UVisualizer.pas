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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UVisualizer.pas $
 * $Id: UVisualizer.pas 2665 2010-10-14 08:00:23Z k-m_schindler $
 *}

unit UVisualizer;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  UGraphicClasses,
  math,
  SysUtils,
  UIni,
  projectM,
  UMusic;

implementation

uses
  Classes,
  IniFiles,
  UAudioConverter,
  UText,
  UCommon,
  UFilesystem,
  UGraphic,
  UMain,
  UConfig,
  UPath,
  UPlatform,
  URenderer,
  URenderer_OpenGL,
  ULog;

{$IF PROJECTM_VERSION >= 4002000}
  {$DEFINE UseProjectMLogging}
{$IFEND}

type
  TProjectMState = ( pmPlay, pmStop, pmPause );

type
  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoVisualization )
    private
      Handle: projectm_handle;
      fInitialized: boolean;
      fProjectMPath: string;
      Presets: array of string;
      PresetOrder: array of cardinal;
      PresetIdx: cardinal;
      ScreenW, ScreenH: integer;

    protected
      procedure RandomPreset(SmoothTransition: boolean);
      procedure AddAudioSamples(Data: PSingle; Count: integer; Channels: projectm_channels);
      procedure RenderFrame();

    public
      function GetName: String;

      function Init(): boolean;
      function SetParameters(ConfigPath: IPath): boolean;
      procedure FindPresets(const Dir: IPath);

      function Finalize(): boolean;

      function Open(const aFileName: IPath): IVideo;
  end;

  TVideo_ProjectM = class( TInterfacedObject, IVideo )
    private
      ProjectM: TVideoPlayback_ProjectM;
      fState: TProjectMState;
      AudioConverter: TAudioConverter;
      fAudioData: array of single;
      DstFormat: TAudioFormatInfo;
      Channels: projectm_channels;
      fRndPCMcount: integer;

      function  GetRandomPCMData(var Data: TPCMData): Cardinal;

    public
      constructor Create(ProjectM: TVideoPlayback_ProjectM);
      destructor Destroy; override;

      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function GetPosition: real;

      procedure SetLoop(Enable: boolean);
      function GetLoop(): boolean;

      procedure SetScreen(Screen: integer);
      function GetScreen(): integer;

      procedure SetScreenPosition(X, Y, Z: double);
      procedure GetScreenPosition(var X, Y, Z: double);

      procedure  SetWidth(Width: double);
      function GetWidth(): double;

      procedure  SetHeight(Height: double);
      function GetHeight(): double;

      procedure SetFrameRange(Range: TRectCoords);
      function GetFrameRange(): TRectCoords;

      function GetFrameAspect(): real;

      procedure SetAspectCorrection(AspectCorrection: TAspectCorrection);
      function GetAspectCorrection(): TAspectCorrection;
              
      procedure SetAlpha(Alpha: double);
      function GetAlpha(): double;

      procedure SetReflectionSpacing(Spacing: double);
      function GetReflectionSpacing(): double;

      procedure GetFrame(Time: Extended);
      procedure Draw();
      procedure DrawReflection();
  end;

procedure PresetChangeRequested(name: PAnsiChar; user_data: pointer); cdecl;
begin
  TVideoPlayback_ProjectM(user_data).RandomPreset(true);
end;

{$IFDEF UseProjectMLogging}
procedure ProjectMLog(message: PAnsiChar; log_level: projectm_log_level; user_data: pointer); cdecl;
begin
  Log.LogError('ProjectM Error: ' + message, 'UVisualizer.ProjectMLog');
end;
{$ENDIF}

{ TVideoPlayback_ProjectM }

function  TVideoPlayback_ProjectM.GetName: String;
begin
  Result := 'ProjectM';
end;

function TVideoPlayback_ProjectM.Init(): boolean;
var
  ProjectMPath: IPath;
begin
  Result := false;
  if (fInitialized) then
  begin
    Result := true;
    Exit;
  end;
  if (not Renderer.SupportsProjectM) then
  begin
    Log.LogError('Renderer does not support ProjectM visualizer', 'TVideoPlayback_ProjectM.Init');
    Exit;
  end;

  {$IFDEF UseProjectMLogging}
  projectm_set_log_callback(@ProjectMLog, true, nil);
  projectm_set_log_level(PROJECTM_LOG_LEVEL_ERROR, true);
  {$ENDIF}

  Handle := projectm_create();
  if (Handle = nil) then
  begin
    Log.LogError('Failed to initialize ProjectM visualizer', 'TVideoPlayback_ProjectM.Init');
    Exit;
  end;

  ProjectMPath := Path(ProjectM_DataDir, pdAppend);
  if not ProjectMPath.IsAbsolute then
    ProjectMPath := Platform.GetGameSharedPath.Append(ProjectMPath);
  fProjectMPath := ProjectMPath.ToNative();

  if (not SetParameters(ProjectMPath.Append('config.inp'))) then
    Exit;
  Randomize;
  PresetOrder := RandomPermute(Length(Presets));
  PresetIdx := 0;
  projectm_set_preset_switch_requested_event_callback(Handle, @PresetChangeRequested, self);
  fInitialized := true;
  Result := true;
end;

function TVideoPlayback_ProjectM.SetParameters(ConfigPath: IPath): boolean;
var
  IniFile: TIniFile;
  PresetPath: IPath;
  MeshX, MeshY: integer;
  SoftCut: double;
  PresetDuration: double;
  EasterEgg: single;
  HardCutSensitivity: single;
  AspectCorrection: boolean;
  Disp: TSDL_DisplayMode;
begin
  Result := false;
  IniFile := TIniFile.Create(ConfigPath.ToNative, [ifoWriteStringBoolean, ifoStripComments]);
  PresetPath := Path(IniFile.ReadString('ProjectM', 'Preset Path', ''));
  if not PresetPath.IsAbsolute then
    PresetPath := Platform.GetGameSharedPath.Append(PresetPath);
  if ((PresetPath.ToUTF8() = '') or (not PresetPath.Exists())) then
  begin
    Log.LogError('Invalid ProjectM Preset Path', 'TVideoPlayback_ProjectM.SetParameters');
    IniFile.Free;
    Exit;
  end;
  FindPresets(PresetPath);
  if (Length(Presets) = 0) then
  begin
    Log.LogError('No ProjectM presets found', 'TVideoPlayback_ProjectM.SetParameters');
    IniFile.Free;
    Exit;
  end;

  MeshX := IniFile.ReadInteger('ProjectM', 'Mesh X', 32);
  MeshY := IniFile.ReadInteger('ProjectM', 'Mesh Y', 24);
  projectm_set_mesh_size(Handle, MeshX, MeshY);

  SDL_GetWindowDisplayMode(screen, @Disp);
  if (Disp.refresh_rate <> 0) then
    projectm_set_fps(Handle, Disp.refresh_rate);

  self.ScreenW := UGraphic.ScreenW;
  self.ScreenH := UGraphic.ScreenH;
  projectm_set_window_size(Handle, self.ScreenW div Screens, self.ScreenH);

  SoftCut := IniFile.ReadFloat('ProjectM', 'Smooth Transition Duration', 5);
  projectm_set_soft_cut_duration(Handle, SoftCut);

  PresetDuration := IniFile.ReadFloat('ProjectM', 'Preset Duration', 1000);
  projectm_set_preset_duration(Handle, PresetDuration);

  EasterEgg := IniFile.ReadFloat('ProjectM', 'Easter Egg Parameter', 1);
  projectm_set_easter_egg(Handle, EasterEgg);

  HardCutSensitivity := IniFile.ReadFloat('ProjectM', 'Hard Cut Sensitivity', 10);
  projectm_set_hard_cut_sensitivity(Handle, HardCutSensitivity);

  AspectCorrection := IniFile.ReadBool('ProjectM', 'Aspect Correction', true);
  projectm_set_aspect_correction(Handle, AspectCorrection);

  IniFile.Free;
  Result := true;
end;

procedure TVideoPlayback_ProjectM.FindPresets(const Dir: IPath);
var
  Extension: IPath;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  FileName: IPath;
  Stream: TFileStream;
begin
  Extension := Path('.milk');
  Iter := FileSystem.FileFind(Dir.Append('*.milk'), 0);
  while Iter.HasNext do
  begin
    FileInfo := Iter.Next;
    FileName := FileInfo.Name;
    if ((FileInfo.Attr and faDirectory) = 0) and Extension.Equals(FileName.GetExtension(), true) then
    begin
      Stream := TFileStream.Create(Dir.Append(FileName).ToNative, fmOpenRead);
      if (Stream.Size > 0) then
      begin
        SetLength(Presets, Length(Presets) + 1);
        SetLength(Presets[High(Presets)], Stream.Size);
        Stream.Read(Presets[High(Presets)][1], Stream.Size);
      end;
      FreeAndNil(Stream);
    end;
  end;
end;

procedure TVideoPlayback_ProjectM.RandomPreset(SmoothTransition: boolean);
begin
  projectm_load_preset_Data(Handle, PChar(Presets[PresetOrder[PresetIdx]]), SmoothTransition);
  PresetIdx := PresetIdx + 1;
  if (PresetIdx > High(Presets)) then
    PresetIdx := 0;
end;

procedure TVideoPlayback_ProjectM.AddAudioSamples(Data: PSingle; Count: integer; Channels: projectm_channels);
begin
  projectm_pcm_add_float(Handle, Data, Count, Channels);
end;

procedure TVideoPlayback_ProjectM.RenderFrame();
begin
  if ((self.ScreenW <> UGraphic.ScreenW) or (self.ScreenH <> UGraphic.ScreenH)) then
  begin
    self.ScreenW := UGraphic.ScreenW;
    self.ScreenH := UGraphic.ScreenH;
    projectm_set_window_size(Handle, self.ScreenW div Screens, self.ScreenH);
  end;
  projectm_opengl_render_frame(Handle);
end;

function TVideoPlayback_ProjectM.Finalize(): boolean;
begin
  projectm_destroy(Handle);
  Result := true;
end;

function TVideoPlayback_ProjectM.Open(const aFileName: IPath): IVideo;
begin
  Result := TVideo_ProjectM.Create(self);
end;

{ TVideo_ProjectM }

constructor TVideo_ProjectM.Create(ProjectM: TVideoPlayback_ProjectM);
var
  SrcFormat: TAudioFormatInfo;
begin
  inherited Create;
  self.ProjectM := ProjectM;
  fRndPCMcount := 0;
  fState := pmStop;
  AudioConverter := TAudioConverter_SWResample.Create;
  SrcFormat := AudioPlayback.GetFormatInfo();
  DstFormat := TAudioFormatInfo.Create(Min(SrcFormat.Channels, 2), SrcFormat.SampleRate, asfFloat);
  AudioConverter.Init(SrcFormat, DstFormat);
  SetLength(fAudioData, AudioConverter.GetOutputBufferSize(SizeOf(TPCMData)) div SizeOf(single));
  if (DstFormat.Channels = 1) then
    Channels := PROJECTM_MONO
  else
    Channels := PROJECTM_STEREO;
end;

destructor TVideo_ProjectM.Destroy;
begin
  Close();
end;

procedure TVideo_ProjectM.Close;
begin
  AudioConverter.Free;
  DstFormat.Free;
end;

procedure TVideo_ProjectM.Play;
begin
  if (fState = pmStop) then
    ProjectM.RandomPreset(false);
  fState := pmPlay;
end;

procedure TVideo_ProjectM.Pause;
begin
  if (fState = pmPlay) then
    fState := pmPause
  else if (fState = pmPause) then
    fState := pmPlay;
end;

procedure TVideo_ProjectM.Stop;
begin
  fState := pmStop;
end;

procedure TVideo_ProjectM.SetPosition(Time: real);
begin
  ProjectM.RandomPreset(false);
end;

function TVideo_ProjectM.GetPosition: real;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.SetLoop(Enable: boolean);
begin
end;

function TVideo_ProjectM.GetLoop(): boolean;
begin
  Result := true;
end;

procedure TVideo_ProjectM.SetScreen(Screen: integer);
begin
end;

function TVideo_ProjectM.GetScreen(): integer;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.SetScreenPosition(X, Y, Z: double);
begin
end;

procedure TVideo_ProjectM.GetScreenPosition(var X, Y, Z: double);
begin
  X := 0;
  Y := 0;
  Z := 0;
end;

procedure TVideo_ProjectM.SetWidth(Width: double);
begin
end;

function TVideo_ProjectM.GetWidth(): double;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.SetHeight(Height: double);
begin
end;

function TVideo_ProjectM.GetHeight(): double;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.SetFrameRange(Range: TRectCoords);
begin
end;

function TVideo_ProjectM.GetFrameRange(): TRectCoords;
begin
  Result.Left := 0;
  Result.Right := 0;
  Result.Upper := 0;
  Result.Lower := 0;
end;

function TVideo_ProjectM.GetFrameAspect(): real;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.SetAspectCorrection(AspectCorrection: TAspectCorrection);
begin
end;

function TVideo_ProjectM.GetAspectCorrection(): TAspectCorrection;
begin
  Result := acoLetterbox;
end;

procedure TVideo_ProjectM.SetAlpha(Alpha: double);
begin
end;

function TVideo_ProjectM.GetAlpha(): double;
begin
  Result := 1;
end;

procedure TVideo_ProjectM.SetReflectionSpacing(Spacing: double);
begin
end;

function TVideo_ProjectM.GetReflectionSpacing(): double;
begin
  Result := 0;
end;

procedure TVideo_ProjectM.GetFrame(Time: Extended);
var
  nSamples: cardinal;
  nBytes: integer;
  Data: TPCMData;
begin
  if (fState <> pmPlay) then
    Exit;

  // get audio data
  nSamples := AudioPlayback.GetPCMData(Data);

  // generate some data if non is available
  if (nSamples = 0) then
    nSamples := GetRandomPCMData(Data);
  nBytes := nSamples * SizeOf(TPCMStereoSample);
  AudioConverter.Convert(PByteArray(@Data[0]), PByteArray(fAudioData), nBytes);

  // send audio-data to projectM
  if (nSamples > 0) then
    ProjectM.AddAudioSamples(PSingle(fAudioData), nSamples, Channels);

  // let projectM render a frame
  ProjectM.RenderFrame();

  Renderer.ResetState;
  Renderer.ClearFrameBuffer(CLEAR_DEPTH);

end;

{**
 * Draws the current frame to screen.
 * TODO: this is not used yet. Data is directly drawn on GetFrame().
 *}
procedure TVideo_ProjectM.Draw();
begin
end;

procedure TVideo_ProjectM.DrawReflection();
begin
end;

{**
 * Produces random "sound"-data in case no audio-data is available.
 * Otherwise the visualization will look rather boring.
 *}
function  TVideo_ProjectM.GetRandomPCMData(var Data: TPCMData): Cardinal;
var
  i: integer;
begin
  // Produce some fake PCM data
  if (fRndPCMcount mod 500 = 0) then
  begin
    FillChar(Data, SizeOf(TPCMData), 0);
  end
  else
  begin
    for i := 0 to 511 do
    begin
      Data[i][0] := Random(High(Word)) - High(Smallint);
      Data[i][1] := Random(High(Word)) - High(Smallint);
    end;
  end;
  Inc(fRndPCMcount);
  Result := 512;
end;

initialization
  MediaManager.Add(TVideoPlayback_ProjectM.Create);

end.
