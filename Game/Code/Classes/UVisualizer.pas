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
  OpenGL12,
  SysUtils,
  UIni,
  {$ifdef DebugDisplay}
  {$ifdef win32}
  dialogs,
  {$endif}
  {$endif}
  projectM,
  UMusic;

implementation

uses
  UGraphic,
  UMain,
  ULog;

var
  singleton_VideoProjectM : IVideoPlayback;

const
  gx           = 32;
  gy           = 24;
  fps          = 30;
  texsize      = 512;
  
var
  ProjectMPath : string;
  presetsDir   : string;
  fontsDir     : string;

  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  inited: boolean;

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

      function    Open( aFileName : string): boolean; // true if succeed
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
  RndPCMcount := 0;
end;


procedure TVideoPlayback_ProjectM.Init();
begin
  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  inited := true;

  ProjectMPath := VisualsPath + 'projectM' + PathDelim;
  presetsDir := ProjectMPath + 'presets';
  fontsDir   := ProjectMPath + 'fonts';

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


function TVideoPlayback_ProjectM.Open( aFileName : string): boolean; // true if succeed
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
  pm.RandomPreset();
end;

function  TVideoPlayback_ProjectM.GetPosition: real;
begin
  result := 0;
end;

procedure TVideoPlayback_ProjectM.SaveOpenGLState();
begin
  // save all OpenGL state-machine attributes
	glPushAttrib(GL_ALL_ATTRIB_BITS);

  // save projection-matrix
  glMatrixMode(GL_PROJECTION);
  // - WARNING: projection-matrix stack-depth is only 2!
  // -> overflow might occur if glPopMatrix() is used for this matrix
  // -> use glGet() instead of glPushMatrix()
  glPushMatrix();
  //glGetDoublev(GL_PROJECTION_MATRIX, @projMatrix);

  // save texture-matrix
  glMatrixMode(GL_TEXTURE);
  // - WARNING: texture-matrix stack-depth is only 2!
  // -> overflow might occur if glPopMatrix() is used for this matrix
  // -> use glGet() instead of glPushMatrix() if problems appear
  glPushMatrix();
  //glGetDoublev(GL_TEXTURE_MATRIX, @texMatrix);

  // save modelview-matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
end;

procedure TVideoPlayback_ProjectM.RestoreOpenGLState();
begin
  // restore projection-matrix
  glMatrixMode(GL_PROJECTION);
  // - WARNING: projection-matrix stack-depth is only 2!
  // -> overflow _occurs_ if glPopMatrix() is used for this matrix
  // -> use glLoadMatrix() instead of glPopMatrix()
  glPopMatrix();
  //glLoadMatrixd(@projMatrix);

  // restore texture-matrix
  // -> overflow might occur if glPopMatrix() is used for this matrix
  glMatrixMode(GL_TEXTURE);
  glPopMatrix();
  //glLoadMatrixd(@texMatrix);

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
  // FIXME: dirty fix needed because the init method is not
  //   called yet.
  if (not inited) then
    Init();

  VisualizerStarted := True;

  pm := TProjectM.Create(gx, gy, fps, texsize, ScreenW, ScreenH,
                         presetsDir, fontsDir);
  //initResult := projectM_initRenderToTexture(pm);

	SaveOpenGLState();
	pm.ResetGL(ScreenW, ScreenH);
  RestoreOpenGLState();
end;

procedure TVideoPlayback_ProjectM.VisualizerStop;
begin
  if VisualizerStarted then begin
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
  if not VisualizerStarted then Exit;
  if VisualizerPaused then Exit;

  // get audio data
  nSamples := AudioPlayback.GetPCMData(PcmData);

  if nSamples = 0 then
    nSamples := GetRandomPCMData(PcmData);

  pm.AddPCM16Data(PSmallint(@PcmData), nSamples);

  // store OpenGL state (might be messed up otherwise)
	SaveOpenGLState();
	pm.ResetGL(ScreenW, ScreenH);

  //glGetIntegerv(GL_PROJECTION_STACK_DEPTH, @stackDepth);
  //writeln('StackDepth0: ' + inttostr(stackDepth));

  // let projectM render a frame
  try
    pm.RenderFrame();
  except
    // this may happen with some presets ( on linux ) if there is a div by zero
    // in projectM's getBeatVals() function (file: beat_detect.cc)
    Log.LogStatus('Div by zero!', 'Visualizer');
    SetPosition( now );
  end;

  //glGetIntegerv(GL_PROJECTION_STACK_DEPTH, @stackDepth);
  //writeln('StackDepth1: ' + inttostr(stackDepth));

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
  // have a nice black background to draw on (even if there were errors opening the vid)
  if Screen=1 then begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  // exit if there's nothing to draw
  if not VisualizerStarted then Exit;

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
  if ( RndPCMcount mod 500 = 0 ) then
  begin
    for i := 0 to 511 do begin
      data[0][i] := 0;
      data[1][i] := 0;
    end;
  end
  else begin
    for i := 0 to 511 do begin
      if ( i mod 2 = 0 ) then begin
        data[0][i] := floor(Random * power(2.,14));
        data[1][i] := floor(Random * power(2.,14));
      end
      else begin;
        data[0][i] := floor(Random * power(2.,14));
        data[1][i] := floor(Random * power(2.,14));
      end;
      if ( i mod 2 = 1 ) then begin
        data[0][i] := -data[0][i];
        data[1][i] := -data[1][i];
      end;
    end;
  end;
  Inc( RndPCMcount );
  result := 512;
end;


initialization
  singleton_VideoProjectM := TVideoPlayback_ProjectM.create();
  AudioManager.add( singleton_VideoProjectM );

finalization
  AudioManager.Remove( singleton_VideoProjectM );



end.
