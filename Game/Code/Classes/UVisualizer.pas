{############################################################################
#                   Visualizer support for UltraStar deluxe                 #
#                                                                           #
#   Created by hennymcc                                                     #
#   Slight modifications by Jay Binks                                       #
#   based on UVideo.pas                                                     #
#############################################################################}

unit UVisualizer;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  UGraphicClasses,
  textgl,
  math,
  gl,
  SysUtils,
  UIni,
  projectM,
  UMusic;

implementation

uses
  UGraphic,
  UMain,
  UConfig,
  ULog;

(*
 * TODO:
 *   - fix video/visualizer switching and initialisation
 *   - use GL_EXT_framebuffer_object for rendering to a separate framebuffer,
 *     this will prevent plugins from messing up our render-context
 *     (-> no stack corruption anymore, no need for Save/RestoreOpenGLState()).
 *   - create a generic (C-compatible) interface for visualization plugins
 *   - create a visualization plugin manager
 *   - write a plugin for projectM in C/C++ (so we need no wrapper anymore)
 *)

var
  singleton_VideoProjectM : IVideoPlayback;

var
  ProjectMPath : string;

  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  inited: boolean;

{$IF PROJECTM_VERSION < 1000000} // < 1.0
const
  meshX = 32;
  meshY = 24;
  fps   = 30;
  textureSize = 512;
{$IFEND}

type
  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoPlayback, IVideoVisualization )
    private
      pm                : TProjectM;

      VisualizerStarted ,
      VisualizerPaused  : Boolean;

      VisualTex         : glUint;
      PCMData           : TPCMData;

      RndPCMcount       : integer;

      projMatrix: array[0..3, 0..3] of GLdouble;
      texMatrix:  array[0..3, 0..3] of GLdouble;

      procedure VisualizerStart;
      procedure VisualizerStop;

      procedure VisualizerTogglePause;

      function  GetRandomPCMData(var data: TPCMData): Cardinal;

      procedure SaveOpenGLState();
      procedure RestoreOpenGLState();

    public
      constructor Create();
      procedure   Init();
      function    GetName: String;

      function    Open(const aFileName : string): boolean; // true if succeed
      procedure   Close;

      procedure   Play;
      procedure   Pause;
      procedure   Stop;

      procedure   SetPosition(Time: real);
      function    GetPosition: real;

      procedure   GetFrame(Time: Extended);
      procedure   DrawGL(Screen: integer);
  end;


constructor TVideoPlayback_ProjectM.Create();
begin
  inherited;
end;


procedure TVideoPlayback_ProjectM.Init();
begin
  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  if (inited) then
    Exit;
  inited := true;

  RndPCMcount := 0;

  ProjectMPath := ProjectM_DataDir + PathDelim;

  VisualizerStarted := False;
  VisualizerPaused  := False;

  {$IFDEF UseTexture}
  glGenTextures(1, PglUint(@VisualTex));
  glBindTexture(GL_TEXTURE_2D, VisualTex);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  {$ENDIF}
end;

function  TVideoPlayback_ProjectM.GetName: String;
begin
  result := 'ProjectM';
end;


function TVideoPlayback_ProjectM.Open(const aFileName : string): boolean; // true if succeed
begin
  VisualizerStart();
  result := true;
end;

procedure TVideoPlayback_ProjectM.Close;
begin
end;

procedure TVideoPlayback_ProjectM.Play;
begin
  VisualizerStart();
end;

procedure TVideoPlayback_ProjectM.Pause;
begin
  VisualizerTogglePause();
end;

procedure TVideoPlayback_ProjectM.Stop;
begin
  VisualizerStop();
end;

procedure TVideoPlayback_ProjectM.SetPosition(Time: real);
begin
  if assigned(pm) then
    pm.NextPreset();
end;

function  TVideoPlayback_ProjectM.GetPosition: real;
begin
  result := 0;
end;

procedure TVideoPlayback_ProjectM.SaveOpenGLState();
begin
  // save all OpenGL state-machine attributes
  glPushAttrib(GL_ALL_ATTRIB_BITS);

  // Note: we do not use glPushMatrix() for the GL_PROJECTION and GL_TEXTURE stacks.
  //   OpenGL specifies the depth of those stacks to be at least 2 but projectM
  //   already uses 2 stack-entries so overflows might be possible on older hardware.
  //   In contrast to this the GL_MODELVIEW stack-size is at least 32, so we can
  //   use glPushMatrix() for this stack.
  
  // save projection-matrix
  glMatrixMode(GL_PROJECTION);
  glGetDoublev(GL_PROJECTION_MATRIX, @projMatrix);
  {$IF (PROJECTM_VERSION >= 1000000) and (PROJECTM_VERSION < 1010000)} // [1.0..1.1)
  // bugfix (1.0 and 1.01): projection-matrix is popped without being pushed first
  glPushMatrix();
  {$IFEND}

  // save texture-matrix
  glMatrixMode(GL_TEXTURE);
  glGetDoublev(GL_TEXTURE_MATRIX, @texMatrix);

  // save modelview-matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  {$IF (PROJECTM_VERSION >= 1000000) and (PROJECTM_VERSION < 1010000)} // [1.0..1.1)
  // bugfix (1.0 and 1.01): modelview-matrix is popped without being pushed first
  glPushMatrix();
  {$IFEND}
end;

procedure TVideoPlayback_ProjectM.RestoreOpenGLState();
begin
  // restore projection-matrix
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixd(@projMatrix);

  // restore texture-matrix
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixd(@texMatrix);

  // restore modelview-matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();

  // restore all OpenGL state-machine attributes
  glPopAttrib();
end;

procedure TVideoPlayback_ProjectM.VisualizerStart;
var
  initResult: Cardinal;
begin
  if VisualizerStarted then
    Exit;

  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  if (not inited) then
    Init();

  {$IF PROJECTM_VERSION >= 1000000} // >= 1.0
  pm := TProjectM.Create(ProjectMPath + 'config.inp');
  {$ELSE}
  pm := TProjectM.Create(
    meshX, meshY, fps, textureSize, ScreenW, ScreenH,
    ProjectMPath + 'presets', ProjectMPath + 'fonts');
  {$IFEND}

  VisualizerStarted := True;

  // skip projectM preset
  pm.RandomPreset();
  // initialize OpenGL
  SaveOpenGLState();
  pm.ResetGL(ScreenW, ScreenH);
  RestoreOpenGLState();
end;

procedure TVideoPlayback_ProjectM.VisualizerStop;
begin
  if VisualizerStarted then
  begin
    VisualizerStarted := False;
    pm.Free();
  end;
end;

procedure TVideoPlayback_ProjectM.VisualizerTogglePause;
begin
  VisualizerPaused := not VisualizerPaused;
end;

procedure TVideoPlayback_ProjectM.GetFrame(Time: Extended);
var
  nSamples: cardinal;
  stackDepth: Integer;
begin
  if not VisualizerStarted then
    Exit;

  if VisualizerPaused then
    Exit;

  // get audio data
  nSamples := AudioPlayback.GetPCMData(PcmData);

  if (nSamples = 0) then
    nSamples := GetRandomPCMData(PcmData);

  if (nSamples > 0) then
    pm.AddPCM16Data(PSmallInt(@PcmData), nSamples);

  // store OpenGL state (might be messed up otherwise)
  SaveOpenGLState();
  pm.ResetGL(ScreenW, ScreenH);

  // let projectM render a frame
  pm.RenderFrame();

  {$IFDEF UseTexture}
  glBindTexture(GL_TEXTURE_2D, VisualTex);
  glFlush();
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, VisualWidth, VisualHeight, 0);
  {$ENDIF}

  // restore USDX OpenGL state
  RestoreOpenGLState();

  // discard projectM's depth buffer information (avoid overlay)
  glClear(GL_DEPTH_BUFFER_BIT);
end;

procedure TVideoPlayback_ProjectM.DrawGL(Screen: integer);
begin
  {$IFDEF UseTexture}
  // have a nice black background to draw on
  if (Screen = 1) then
  begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  
  // exit if there's nothing to draw
  if not VisualizerStarted then
    Exit;

  // setup display
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  gluOrtho2D(0, 1, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glBindTexture(GL_TEXTURE_2D, VisualTex);
  glColor4f(1, 1, 1, 1);

  // draw projectM frame
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(0, 0);
    glTexCoord2f(1, 0); glVertex2f(1, 0);
    glTexCoord2f(1, 1); glVertex2f(1, 1);
    glTexCoord2f(0, 1); glVertex2f(0, 1);
  glEnd();

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

  // restore state
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
  {$ENDIF}
end;

function  TVideoPlayback_ProjectM.GetRandomPCMData(var data: TPCMData): Cardinal;
var
  i: integer;
begin
  // Produce some fake PCM data
  if (RndPCMcount mod 500 = 0) then
  begin
    FillChar(data, SizeOf(TPCMData), 0);
  end
  else
  begin
    for i := 0 to 511 do
    begin
      data[i][0] := Random(High(Word)+1);
      data[i][1] := Random(High(Word)+1);
    end;
  end;
  Inc(RndPCMcount);
  result := 512;
end;


initialization
  singleton_VideoProjectM := TVideoPlayback_ProjectM.create();
  AudioManager.add( singleton_VideoProjectM );

finalization
  AudioManager.Remove( singleton_VideoProjectM );



end.
