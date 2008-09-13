unit UVisualizer;

(* TODO:
 *   - fix video/visualizer switching
 *   - use GL_EXT_framebuffer_object for rendering to a separate framebuffer,
 *     this will prevent plugins from messing up our render-context
 *     (-> no stack corruption anymore, no need for Save/RestoreOpenGLState()).
 *   - create a generic (C-compatible) interface for visualization plugins
 *   - create a visualization plugin manager
 *   - write a plugin for projectM in C/C++ (so we need no wrapper anymore)
 *)

{* Note:
 * It would be easier to create a seperate Render-Context (RC) for projectM
 * and switch to it when necessary. This can be achieved by pbuffers
 * (slow and platform specific) or the OpenGL FramebufferObject (FBO) extension
 * (fast and plattform-independent but not supported by older graphic-cards/drivers).
 *
 * See http://oss.sgi.com/projects/ogl-sample/registry/EXT/framebuffer_object.txt
 *
 * To support as many cards as possible we will stick to the current dirty
 * solution for now even if it is a pain to save/restore projectM's state due
 * to bugs etc.
 *
 * This also restricts us to projectM. As other plug-ins might have different
 * needs and bugs concerning the OpenGL state, USDX's state would probably be
 * corrupted after the plug-in finshed drawing.
 *}

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

{$IF PROJECTM_VERSION < 1000000} // < 1.0
// Initialization data used on projectM 0.9x creation.
// Since projectM 1.0 this data is passed via the config-file.
const
  meshX = 32;
  meshY = 24;
  fps   = 30;
  textureSize = 512;
{$IFEND}

type
  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoPlayback, IVideoVisualization )
    private
      pm: TProjectM;
      ProjectMPath : string;
      Initialized: boolean;

      VisualizerStarted: boolean;
      VisualizerPaused: boolean;

      VisualTex: GLuint;
      PCMData: TPCMData;
      RndPCMcount: integer;

      ProjMatrix: array[0..3, 0..3] of GLdouble;
      TexMatrix:  array[0..3, 0..3] of GLdouble;

      procedure VisualizerStart;
      procedure VisualizerStop;

      procedure VisualizerTogglePause;

      function  GetRandomPCMData(var Data: TPCMData): Cardinal;

      procedure SaveOpenGLState();
      procedure RestoreOpenGLState();

    public
      function GetName: String;

      function Init(): boolean;
      function Finalize(): boolean;

      function Open(const aFileName : string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function GetPosition: real;

      procedure GetFrame(Time: Extended);
      procedure DrawGL(Screen: integer);
  end;


function  TVideoPlayback_ProjectM.GetName: String;
begin
  Result := 'ProjectM';
end;

function TVideoPlayback_ProjectM.Init(): boolean;
begin
  Result := true;

  if (Initialized) then
    Exit;
  Initialized := true;

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

function TVideoPlayback_ProjectM.Finalize(): boolean;
begin
  VisualizerStop();
  {$IFDEF UseTexture}
  glDeleteTextures(1, PglUint(@VisualTex));
  {$ENDIF}
  Result := true;
end;

function TVideoPlayback_ProjectM.Open(const aFileName : string): boolean; // true if succeed
begin
  Result := false;
end;

procedure TVideoPlayback_ProjectM.Close;
begin
  VisualizerStop();
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
    pm.RandomPreset();
end;

function  TVideoPlayback_ProjectM.GetPosition: real;
begin
  Result := 0;
end;

{**
 * Saves the current OpenGL state.
 * This is necessary to prevent projectM from corrupting USDX's current
 * OpenGL state.
 *
 * The following steps are performed:
 *   - All attributes are pushed to the attribute-stack
 *   - Projection-/Texture-matrices are saved
 *   - Modelview-matrix is pushed to the Modelview-stack
 *   - the OpenGL error-state (glGetError) is cleared
 *}
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
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);
  {$IF PROJECTM_VERSION = 1000000} // 1.0, 1.01
  // bugfix: projection-matrix is popped without being pushed first
  glPushMatrix();
  {$IFEND}

  // save texture-matrix
  glMatrixMode(GL_TEXTURE);
  glGetDoublev(GL_TEXTURE_MATRIX, @TexMatrix);

  // save modelview-matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  {$IF PROJECTM_VERSION = 1000000} // 1.0, 1.01
  // bugfix: modelview-matrix is popped without being pushed first
  glPushMatrix();
  {$IFEND}

  // reset OpenGL error-state
  glGetError();
end;

{**
 * Restores the OpenGL state saved by SaveOpenGLState()
 * and resets the error-state.
 *}
procedure TVideoPlayback_ProjectM.RestoreOpenGLState();
begin
  // reset OpenGL error-state
  glGetError();

  // restore projection-matrix
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixd(@ProjMatrix);

  // restore texture-matrix
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixd(@TexMatrix);

  // restore modelview-matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();

  // restore all OpenGL state-machine attributes
  glPopAttrib();
end;

procedure TVideoPlayback_ProjectM.VisualizerStart;
begin
  if VisualizerStarted then
    Exit;

  // the OpenGL state must be saved before
  SaveOpenGLState();
  try

    try
      {$IF PROJECTM_VERSION >= 1000000} // >= 1.0
      pm := TProjectM.Create(ProjectMPath + 'config.inp');
      {$ELSE}
      pm := TProjectM.Create(
        meshX, meshY, fps, textureSize, ScreenW, ScreenH,
        ProjectMPath + 'presets', ProjectMPath + 'fonts');
      {$IFEND}
    except on E: Exception do
      begin
        // Create() might fail if the config-file is not found
        Log.LogError('TProjectM.Create: ' + E.Message, 'TVideoPlayback_ProjectM.VisualizerStart');
        Exit;
      end;
    end;

    // initialize OpenGL
    pm.ResetGL(ScreenW, ScreenH);
    // skip projectM default-preset
    pm.RandomPreset();
    // projectM >= 1.0 uses the OpenGL FramebufferObject (FBO) extension.
    // Unfortunately it does NOT reset the framebuffer-context after
    // TProjectM.Create. Either glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0) for
    // a manual reset or TProjectM.RenderFrame() must be called.
    // We use the latter so we do not need to load the FBO extension in USDX.
    pm.RenderFrame();

    VisualizerPaused := false;
    VisualizerStarted := true;
  finally
    RestoreOpenGLState();
  end;
end;

procedure TVideoPlayback_ProjectM.VisualizerStop;
begin
  if VisualizerStarted then
  begin
    VisualizerPaused := false;
    VisualizerStarted := false;
    FreeAndNil(pm);
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

  // generate some data if non is available
  if (nSamples = 0) then
    nSamples := GetRandomPCMData(PcmData);

  // send audio-data to projectM
  if (nSamples > 0) then
    pm.AddPCM16Data(PSmallInt(@PcmData), nSamples);

  // store OpenGL state (might be messed up otherwise)
  SaveOpenGLState();
  try
    // setup projectM's OpenGL state
    pm.ResetGL(ScreenW, ScreenH);

    // let projectM render a frame
    pm.RenderFrame();

    {$IFDEF UseTexture}
    glBindTexture(GL_TEXTURE_2D, VisualTex);
    glFlush();
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, VisualWidth, VisualHeight, 0);
    {$ENDIF}
  finally
    // restore USDX OpenGL state
    RestoreOpenGLState();
  end;

  // discard projectM's depth buffer information (avoid overlay)
  glClear(GL_DEPTH_BUFFER_BIT);
end;

{**
 * Draws the current frame to screen.
 * TODO: this is not used yet. Data is directly drawn on GetFrame().
 *}
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

{**
 * Produces random "sound"-data in case no audio-data is available.
 * Otherwise the visualization will look rather boring.
 *}
function  TVideoPlayback_ProjectM.GetRandomPCMData(var Data: TPCMData): Cardinal;
var
  i: integer;
begin
  // Produce some fake PCM data
  if (RndPCMcount mod 500 = 0) then
  begin
    FillChar(Data, SizeOf(TPCMData), 0);
  end
  else
  begin
    for i := 0 to 511 do
    begin
      Data[i][0] := Random(High(Word)+1);
      Data[i][1] := Random(High(Word)+1);
    end;
  end;
  Inc(RndPCMcount);
  Result := 512;
end;


initialization
  MediaManager.Add(TVideoPlayback_ProjectM.Create);

end.
