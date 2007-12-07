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

uses SDL,
     UGraphicClasses,
     textgl,
     math,
     OpenGL12,
     SysUtils,
     UIni,
     {$ifdef DebugDisplay}
     dialogs,
     {$ENDIF}
     projectM,
     UMusic;

implementation

uses
     UGraphic;

var
  singleton_VideoProjectM : IVideoPlayback;

const
  gx           = 32;
  gy           = 24;
  fps          = 30;
  texsize      = 512;
  visuals_Dir  = 'Visuals'; // TODO: move this to a place common for all visualizers
  projectM_Dir = visuals_Dir+'/projectM';

type
  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoPlayback, IVideoVisualization )

    pm                : PProjectM;

    VisualizerStarted ,
    VisualizerPaused  : Boolean;

    VisualTex         : glUint;
    PCMData           : TPCMData;
    hRC               : Integer;
    hDC               : Integer;

    RndPCMcount       : integer;

    procedure VisualizerStart;
    procedure VisualizerStop;

    procedure VisualizerTogglePause;
    
    function  GetRandomPCMData(var data: TPCMData): Cardinal;
  public
    constructor create();
    procedure   init();
    function    GetName: String;    

    function    Open( aFileName : string): boolean; // true if succeed
    procedure   Close;

    procedure   Play;
    procedure   Pause;
    procedure   Stop;

    procedure   MoveTo(Time: real);
    function    getPosition: real;

    procedure   GetFrame(Time: Extended);
    procedure   DrawGL(Screen: integer);  
  end;  


constructor TVideoPlayback_ProjectM.create();
begin
  RndPCMcount := 0;
end;


procedure TVideoPlayback_ProjectM.init();
begin
  VisualizerStarted := False;
  VisualizerPaused  := False;

  glGenTextures(1, PglUint(@VisualTex));
	glBindTexture(GL_TEXTURE_2D, VisualTex);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
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

procedure TVideoPlayback_ProjectM.MoveTo(Time: real);
begin
  // this code MAY be able to be cut down... but Im not 100% sure..
  // in here, we cant realy move to a specific time, since its all random generated
  // but a call to this function will change the preset, which changes the pattern 
	projectM_reset(pm);

	pm^.fullscreen := 0;
  pm^.renderTarget^.texsize := texsize;
	pm^.gx := gx;
	pm^.gy := gy;
	pm^.fps := fps;
	pm^.renderTarget^.usePbuffers := 0;

	pm^.fontURL := PChar(projectM_Dir+'/fonts');
	pm^.presetURL := PChar(projectM_Dir+'/presets');

	glPushAttrib(GL_ALL_ATTRIB_BITS);
	projectM_init(pm);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMatrixMode(GL_TEXTURE);
  glPushMatrix();

	projectM_resetGL(pm, ScreenW, ScreenH);
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
  glMatrixMode(GL_TEXTURE);
  glPopMatrix();
  glPopAttrib();

end;

function  TVideoPlayback_ProjectM.getPosition: real;
begin
  result := 0;  
end;

procedure TVideoPlayback_ProjectM.VisualizerStart;
begin
  VisualizerStarted := True;

  New(pm);
	projectM_reset(pm);

	pm^.fullscreen := 0;
  pm^.renderTarget^.texsize := texsize;
	pm^.gx := gx;
	pm^.gy := gy;
	pm^.fps := fps;
	pm^.renderTarget^.usePbuffers := 0;

	pm^.fontURL   := PChar(projectM_Dir+'/fonts');
	pm^.presetURL := PChar(projectM_Dir+'/presets');

	glPushAttrib(GL_ALL_ATTRIB_BITS);
	projectM_init(pm);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMatrixMode(GL_TEXTURE);
  glPushMatrix();

	projectM_resetGL(pm, ScreenW, ScreenH);
  
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
  glMatrixMode(GL_TEXTURE);
  glPopMatrix();
  glPopAttrib();
end;

procedure TVideoPlayback_ProjectM.VisualizerStop;
begin
  if VisualizerStarted then begin
    VisualizerStarted := False;
    Dispose(pm);
  end;
end;

procedure TVideoPlayback_ProjectM.VisualizerTogglePause;
begin
  VisualizerPaused := not VisualizerPaused;
end;

procedure TVideoPlayback_ProjectM.GetFrame(Time: Extended);
var
  i: integer;
  nSamples: cardinal;
begin
  if not VisualizerStarted then Exit;
  if VisualizerPaused then Exit;

  // get audio data
  nSamples := AudioPlayback.GetPCMData(PcmData);

  if nSamples = 0 then
    nSamples := GetRandomPCMData(PcmData);
  
  addPCM16Data(PPCM16Data(@PcmData), nSamples);

  // store OpenGL state (might be messed up otherwise)
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMatrixMode(GL_TEXTURE);
  glPushMatrix();

  // let projectM render a frame
  try
    renderFrame(pm);
  except
    // This happens with some presets... ( and only some times also .. moreso on linux )
    // if we have an "Invalid Floating Point Error" while rendering a frame... then change preset.
    MoveTo( now );
    
    // hmm have to be careful, that we dont get to many here... coz it could keep failing.. and trying again.
  end;
  glFlush();

  {$IFDEF UseTexture}
  glBindTexture(GL_TEXTURE_2D, VisualTex);
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, VisualWidth, VisualHeight, 0);
  {$ENDIF}

  // restore USDX OpenGL state
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
  glMatrixMode(GL_TEXTURE);
  glPopMatrix();
  glPopAttrib();

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

  {
  glbegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(400-VisualWidth/2, 300-VisualHeight/2);
    glTexCoord2f(0, 1);
    glVertex2f(400-VisualWidth/2, 300+VisualHeight/2);
    glTexCoord2f(1, 1);
    glVertex2f(400+VisualWidth/2, 300+VisualHeight/2);
    glTexCoord2f(1, 0);
    glVertex2f(400+VisualWidth/2, 300-VisualHeight/2);
  glEnd;
  }

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
