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
     UMusic,
     windows;

implementation

var
  singleton_VideoProjectM : IVideoPlayback;

const
  VisualWidth  = 800;  // 640
  VisualHeight = 600;  // 480
  gx           = 32;
  gy           = 24;
  fps          = 30;
  texsize      = 512;

type
  TVideoPlayback_ProjectM = class( TInterfacedObject, IVideoPlayback )

    pm                : PProjectM;

    VisualizerStarted ,
    VisualizerPaused  : Boolean;

    VisualTex         : glUint;
    pcm_data          : TPCM16;
    hRC               : Integer;
    hDC               : Integer;

    procedure VisualizerStart;
    procedure VisualizerStop;

    procedure VisualizerTogglePause;
  public
    constructor create();
    function  GetName: String;

    procedure init();

    function  Open( aFileName : string): boolean; // true if succeed
    procedure Close;

    procedure Play;
    procedure Pause;
    procedure Stop;

    procedure MoveTo(Time: real);
    function  getPosition: real;

    procedure GetFrame(Time: Extended); // WANT TO RENAME THESE TO BE MORE GENERIC
    procedure DrawGL(Screen: integer);  // WANT TO RENAME THESE TO BE MORE GENERIC
  end;  


constructor TVideoPlayback_ProjectM.create();
begin
end;


procedure TVideoPlayback_ProjectM.init();
begin
  writeln( 'TVideoPlayback_ProjectM - INITIALIZE !!!!!!!!' );

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
end;

function  TVideoPlayback_ProjectM.getPosition: real;
begin
  result := 0;  
end;

procedure TVideoPlayback_ProjectM.VisualizerStart;
begin
//exit;
  VisualizerStarted := True;

  New(pm);
	projectM_reset(pm);

	pm^.fullscreen := 0;
  pm^.renderTarget^.texsize := texsize;
	pm^.gx := gx;
	pm^.gy := gy;
	pm^.fps := fps;
	pm^.renderTarget^.usePbuffers := 0;

	pm^.fontURL   := PChar('Visuals\fonts');
	pm^.presetURL := PChar('Visuals\presets');

	glPushAttrib(GL_ALL_ATTRIB_BITS);
	projectM_init(pm);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMatrixMode(GL_TEXTURE);
  glPushMatrix();
	projectM_resetGL(pm, VisualWidth, VisualHeight);
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
  if VisualizerPaused then VisualizerPaused:=False
  else VisualizerPaused:=True;
end;

procedure TVideoPlayback_ProjectM.GetFrame(Time: Extended);
var
  i: integer;
begin
//  exit;

  if not VisualizerStarted then Exit;
  if VisualizerPaused then Exit;

  Randomize();

	for i := 0 to 511 do
  begin
    pcm_data[0][i] := RandomRange(High(Smallint), Low(Smallint));
    pcm_data[1][i] := pcm_data[0][i];
  end;
  addPCM16(pcm_data);

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMatrixMode(GL_TEXTURE);
  glPushMatrix();
  renderFrame(pm);
  glFlush();
  {
  glBindTexture(GL_TEXTURE_2D, VisualTex);
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, VisualWidth, VisualHeight, 0);
  }
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
  glMatrixMode(GL_TEXTURE);
  glPopMatrix();
  glPopAttrib();

  glClear(GL_DEPTH_BUFFER_BIT);

  {
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glEnable(GL_TEXTURE_2D);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glBindTexture(GL_TEXTURE_2D, VisualTex);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(-1, -1);
    glTexCoord2f(1, 0); glVertex2f( 1, -1);
    glTexCoord2f(1, 1); glVertex2f( 1,  1);
    glTexCoord2f(0, 1); glVertex2f(-1,  1);
  glEnd();
  glDisable(GL_TEXTURE_2D);
  }
end;

procedure TVideoPlayback_ProjectM.DrawGL(Screen: integer);
begin

  exit;

  // have a nice black background to draw on (even if there were errors opening the vid)
  if Screen=1 then begin
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  // exit if there's nothing to draw
  if not VisualizerStarted then Exit;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, VisualTex);
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
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

end;


initialization
  singleton_VideoProjectM := TVideoPlayback_ProjectM.create();

  writeln( 'UVideoProjectM - Register Playback' );
  AudioManager.add( singleton_VideoProjectM );

finalization
  AudioManager.Remove( singleton_VideoProjectM );



end.
