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

{.$DEFINE UseTexture}

uses
  SDL,
  UGraphicClasses,
  textgl,
  math,
  gl,
  {$IFDEF UseTexture}
  glu,
  {$ENDIF}
  SysUtils,
  UIni,
  projectM,
  UMusic;

implementation

uses
  UGraphic,
  UMain,
  UConfig,
  UPath,
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
  TProjectMState = ( pmPlay, pmStop, pmPause );

type
  TGLMatrix = array[0..3, 0..3] of GLdouble;
  TGLMatrixStack = array of TGLMatrix;

type
  TVideo_ProjectM = class( TInterfacedObject, IVideo )
    private
      fPm: TProjectM;
      fProjectMPath : string;

      fState: TProjectMState;

      fScreen:  integer;

      fVisualTex: GLuint;
      fPCMData: TPCMData;
      fRndPCMcount: integer;

      fModelviewMatrixStack: TGLMatrixStack;
      fProjectionMatrixStack: TGLMatrixStack;
      fTextureMatrixStack:  TGLMatrixStack;

      procedure InitProjectM;

      function  GetRandomPCMData(var Data: TPCMData): Cardinal;

      function GetMatrixStackDepth(MatrixMode: GLenum): GLint;
      procedure SaveMatrixStack(MatrixMode: GLenum; var MatrixStack: TGLMatrixStack);
      procedure RestoreMatrixStack(MatrixMode: GLenum; var MatrixStack: TGLMatrixStack);
      procedure SaveOpenGLState();
      procedure RestoreOpenGLState();

    public
      constructor Create;
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

  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoVisualization )
    private
      fInitialized: boolean;

    public
      function GetName: String;

      function Init(): boolean;
      function Finalize(): boolean;

      function Open(const aFileName: IPath): IVideo;
  end;


{ TVideoPlayback_ProjectM }

function  TVideoPlayback_ProjectM.GetName: String;
begin
  Result := 'ProjectM';
end;

function TVideoPlayback_ProjectM.Init(): boolean;
begin
  Result := true;
  if (fInitialized) then
    Exit;
  fInitialized := true;
end;

function TVideoPlayback_ProjectM.Finalize(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_ProjectM.Open(const aFileName: IPath): IVideo;
begin
  Result := TVideo_ProjectM.Create;
end;


{ TVideo_ProjectM }

constructor TVideo_ProjectM.Create;
begin
  fRndPCMcount := 0;

  fProjectMPath := ProjectM_DataDir + PathDelim;

  fState := pmStop;

  {$IFDEF UseTexture}
  glGenTextures(1, PglUint(@fVisualTex));
  glBindTexture(GL_TEXTURE_2D, fVisualTex);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  {$ENDIF}

  InitProjectM();
end;

destructor TVideo_ProjectM.Destroy;
begin
  Close();
  {$IFDEF UseTexture}
  glDeleteTextures(1, PglUint(@fVisualTex));
  {$ENDIF}
end;

procedure TVideo_ProjectM.Close;
begin
  FreeAndNil(fPm);
end;

procedure TVideo_ProjectM.Play;
begin
  if (fState = pmStop) and (assigned(fPm)) then
    fPm.RandomPreset();
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
  if assigned(fPm) then
    fPm.RandomPreset();
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
  Result := acoStretch;
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

{**
 * Returns the stack depth of the given OpenGL matrix mode stack.
 *}
function TVideo_ProjectM.GetMatrixStackDepth(MatrixMode: GLenum): GLint;
begin
  // get number of matrices on stack
  case (MatrixMode) of
    GL_PROJECTION:
      glGetIntegerv(GL_PROJECTION_STACK_DEPTH, @Result);
    GL_MODELVIEW:
      glGetIntegerv(GL_MODELVIEW_STACK_DEPTH, @Result);
    GL_TEXTURE:
      glGetIntegerv(GL_TEXTURE_STACK_DEPTH, @Result);
  end;
end;

{**
 * Saves the current matrix stack using MatrixMode
 * (one of GL_PROJECTION/GL_TEXTURE/GL_MODELVIEW)
 *
 * Use this function instead of just saving the current matrix with glPushMatrix().
 * OpenGL specifies the depth of the GL_PROJECTION and GL_TEXTURE stacks to be
 * at least 2 but projectM already uses 2 stack-entries so overflows might be
 * possible on older hardware.
 * In contrast to this the GL_MODELVIEW stack-size is at least 32, but this
 * function should be used for the modelview stack too. We cannot rely on a
 * proper stack management of the underlying visualizer (projectM).
 * For example in the projectM versions 1.0 - 1.01 the modelview- and
 * projection-matrices were popped without being pushed first.
 *
 * By saving the whole stack we are on the safe side, so a nasty bug in the
 * visualizer does not corrupt USDX.
 *}
procedure TVideo_ProjectM.SaveMatrixStack(MatrixMode: GLenum;
                var MatrixStack: TGLMatrixStack);
var
  I: integer;
  StackDepth: GLint;
begin
  glMatrixMode(MatrixMode);

  StackDepth := GetMatrixStackDepth(MatrixMode);
  SetLength(MatrixStack, StackDepth);

  // save current matrix stack
  for I := StackDepth-1 downto 0 do
  begin
    // save current matrix
    case (MatrixMode) of
      GL_PROJECTION:
        glGetDoublev(GL_PROJECTION_MATRIX, @MatrixStack[I]);
      GL_MODELVIEW:
        glGetDoublev(GL_MODELVIEW_MATRIX, @MatrixStack[I]);
      GL_TEXTURE:
        glGetDoublev(GL_TEXTURE_MATRIX, @MatrixStack[I]);
    end;

    // remove matrix from stack
    if (I > 0) then
      glPopMatrix();
  end;

  // reset default (first) matrix
  glLoadIdentity();
end;

{**
 * Restores the OpenGL matrix stack stored with SaveMatrixStack.
 *}
procedure TVideo_ProjectM.RestoreMatrixStack(MatrixMode: GLenum;
                var MatrixStack: TGLMatrixStack);
var
  I: integer;
  StackDepth: GLint;
begin
  glMatrixMode(MatrixMode);

  StackDepth := GetMatrixStackDepth(MatrixMode);
  // remove all (except the first) matrices from current stack
  for I := 1 to StackDepth-1 do
    glPopMatrix();

  // rebuild stack
  for I := 0 to High(MatrixStack) do
  begin
    glLoadMatrixd(@MatrixStack[I]);
    if (I < High(MatrixStack)) then
      glPushMatrix();
  end;

  // clean stored stack
  SetLength(MatrixStack, 0);
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
procedure TVideo_ProjectM.SaveOpenGLState();
begin
  // save all OpenGL state-machine attributes
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  SaveMatrixStack(GL_PROJECTION, fProjectionMatrixStack);
  SaveMatrixStack(GL_MODELVIEW, fModelviewMatrixStack);
  SaveMatrixStack(GL_TEXTURE, fTextureMatrixStack);

  glMatrixMode(GL_MODELVIEW);

  // reset OpenGL error-state
  glGetError();
end;

{**
 * Restores the OpenGL state saved by SaveOpenGLState()
 * and resets the error-state.
 *}
procedure TVideo_ProjectM.RestoreOpenGLState();
begin
  // reset OpenGL error-state
  glGetError();

  // restore matrix stacks
  RestoreMatrixStack(GL_PROJECTION, fProjectionMatrixStack);
  RestoreMatrixStack(GL_MODELVIEW, fModelviewMatrixStack);
  RestoreMatrixStack(GL_TEXTURE, fTextureMatrixStack);

  // restore all OpenGL state-machine attributes
  // (also restores the matrix mode)
  glPopClientAttrib();
  glPopAttrib();
end;

procedure TVideo_ProjectM.InitProjectM;
begin
  // the OpenGL state must be saved before TProjectM.Create is called
  SaveOpenGLState();
  try

    try
      {$IF PROJECTM_VERSION >= 1000000} // >= 1.0
      fPm := TProjectM.Create(fProjectMPath + 'config.inp');
      {$ELSE}
      fPm := TProjectM.Create(
        meshX, meshY, fps, textureSize, ScreenW, ScreenH,
        fProjectMPath + 'presets', fProjectMPath + 'fonts');
      {$IFEND}
    except on E: Exception do
      begin
        // Create() might fail if the config-file is not found
        Log.LogError('TProjectM.Create: ' + E.Message, 'TVideoPlayback_ProjectM.VisualizerStart');
        Exit;
      end;
    end;

    // initialize OpenGL
    fPm.ResetGL(ScreenW, ScreenH);
    // skip projectM default-preset
    fPm.RandomPreset();
    // projectM >= 1.0 uses the OpenGL FramebufferObject (FBO) extension.
    // Unfortunately it does NOT reset the framebuffer-context after
    // TProjectM.Create. Either glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0) for
    // a manual reset or TProjectM.RenderFrame() must be called.
    // We use the latter so we do not need to load the FBO extension in USDX.
    fPm.RenderFrame();
  finally
    RestoreOpenGLState();
  end;
end;

procedure TVideo_ProjectM.GetFrame(Time: Extended);
var
  nSamples: cardinal;
begin
  if (fState <> pmPlay) then
    Exit;

  // get audio data
  nSamples := AudioPlayback.GetPCMData(fPCMData);

  // generate some data if non is available
  if (nSamples = 0) then
    nSamples := GetRandomPCMData(fPCMData);

  // send audio-data to projectM
  if (nSamples > 0) then
    fPm.AddPCM16Data(PSmallInt(@fPCMData), nSamples);

  // store OpenGL state (might be messed up otherwise)
  SaveOpenGLState();
  try
    // setup projectM's OpenGL state
    fPm.ResetGL(ScreenW, ScreenH);

    // let projectM render a frame
    fPm.RenderFrame();

    {$IFDEF UseTexture}
    glBindTexture(GL_TEXTURE_2D, fVisualTex);
    glFlush();
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, fVisualWidth, fVisualHeight, 0);
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
procedure TVideo_ProjectM.Draw();
begin
  {$IFDEF UseTexture}
  // have a nice black background to draw on
  if (fScreen = 1) then
  begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;

  // exit if there's nothing to draw
  if (fState <> pmPlay) then
    Exit;

  // setup display
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  // Use count of screens instead of 1 for the right corner
  // otherwise we would draw the visualization streched over both screens
  // another point is that we draw over the at this time drawn first
  // screen, if Screen = 2
  gluOrtho2D(0, Screens, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glBindTexture(GL_TEXTURE_2D, fVisualTex);
  glColor4f(1, 1, 1, 1);

  // draw projectM frame
  // Screen is 1 to 2. So current screen is from (Screen - 1) to (Screen)
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f((fScreen - 1), 0);
    glTexCoord2f(1, 0); glVertex2f(fScreen, 0);
    glTexCoord2f(1, 1); glVertex2f(fScreen, 1);
    glTexCoord2f(0, 1); glVertex2f((fScreen - 1), 1);
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
      Data[i][0] := Random(High(Word)+1);
      Data[i][1] := Random(High(Word)+1);
    end;
  end;
  Inc(fRndPCMcount);
  Result := 512;
end;


initialization
  MediaManager.Add(TVideoPlayback_ProjectM.Create);

end.
