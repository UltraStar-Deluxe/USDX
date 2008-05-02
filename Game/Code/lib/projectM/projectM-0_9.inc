{$IFDEF Unix}
uses
  baseunix;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  libprojectM = 'libprojectM.dll';
{$ELSE}
  libprojectM = 'libprojectM.so';
{$ENDIF}

{**************** INTERNAL SECTION ****************}


type
  PPSingle = ^PSingle;

type
  _TContextType = Integer;
const
  AGL_CONTEXT   = 0;
  CGL_CONTEXT   = 1;
  NSGL_CONTEXT  = 2;
  GLX_CONTEXT   = 3;
  WGL_CONTEXT   = 4;

type
  _PRenderTarget = ^_TRenderTarget;
  _TRenderTarget = record
    { Texture size }
    texsize: Integer;

    { Application context }
    origContextType: _TContextType;

    usePbuffers: Integer;

    {$ifdef LINUX}
      lock_func: procedure(); cdecl;
      unlock_func: procedure(); cdecl;
    {$endif}

    { Opaque pbuffer context and pbuffer }
    {$ifdef DARWIN}
      origContext: Pointer;
      pbufferContext: Pointer;
      pbuffer: Pointer;
    {$endif}

  { Render target texture ID for non-pbuffer systems }
    textureID: array[0..2] of GLuint;
  end;

  _PProjectM = ^_TProjectM;
  _TProjectM = record
    presetURL: PChar;
    presetName: PChar;
    fontURL: PChar;

    hasInit: Integer;

    noSwitch: Integer;
    pcmframes: Integer;
    freqframes: Integer;
    totalframes: Integer;

    showfps: Integer;
    showtitle: Integer;
    showpreset: Integer;
    showhelp: Integer;
    showstats: Integer;

    studio: Integer;

    fbuffer: PGLubyte;

    {$IFNDEF MSWINDOWS}
    { The first ticks value of the application }
    startTime: timeval;
    {$ELSE}
    startTime: Longint;
    {$ENDIF}
    Time: Single;

    { Render target texture ID }
    renderTarget: _PRenderTarget;

    disp: array[0..79] of Char;

    wave_o: Single;

    //int texsize=1024;   //size of texture to do actual graphics
    fvw: Integer;   //fullscreen dimensions
    fvh: Integer;
    wvw: Integer;   //windowed dimensions
    wvh: Integer;
    vw: Integer;    //runtime dimensions
    vh: Integer;
    fullscreen: Integer;

    maxsamples: Integer;  //size of PCM buffer
    numsamples: Integer;  //size of new PCM info
    pcmdataL: PSingle;     //holder for most recent pcm data
    pcmdataR: PSingle;     //holder for most recent pcm data

    avgtime: Integer;     //# frames per preset
    
    title: PChar;
    drawtitle: Integer;

    correction: Integer;

    vol: Single;

    //per pixel equation variables
    gridx: PPSingle;     //grid containing interpolated mesh
    gridy: PPSingle;
    origtheta: PPSingle; //grid containing interpolated mesh reference values
    origrad: PPSingle;
    origx: PPSingle;     //original mesh
    origy: PPSingle;
    origx2: PPSingle;    //original mesh
    origy2: PPSingle;

    { Timing information }
    mspf: Integer;
    timed: Integer;
    timestart: Integer;
    nohard: Integer;
    count: Integer;
    realfps,
      fpsstart: Single;

    { PCM data }
    vdataL: array[0..511] of Single;  //holders for FFT data (spectrum)
    vdataR: array[0..511] of Single;

    { Various toggles }
    doPerPixelEffects: Integer;
    doIterative: Integer;

    { ENGINE VARIABLES }
    { From engine_vars.h }
    preset_name: array[0..255] of Char;

    { PER FRAME CONSTANTS BEGIN }
    zoom: Single;
    zoomexp: Single;
    rot: Single;
    warp: Single;

    sx: Single;
    sy: Single;
    dx: Single;
    dy: Single;
    cx: Single;
    cy: Single;

    gy: Integer;
    gx: Integer;

    decay: Single;

    wave_r: Single;
    wave_g: Single;
    wave_b: Single;
    wave_x: Single;
    wave_y: Single;
    wave_mystery: Single;

    ob_size: Single;
    ob_r: Single;
    ob_g: Single;
    ob_b: Single;
    ob_a: Single;

    ib_size: Single;
    ib_r: Single;
    ib_g: Single;
    ib_b: Single;
    ib_a: Single;

    meshx: Integer;
    meshy: Integer;

    mv_a: Single;
    mv_r: Single;
    mv_g: Single;
    mv_b: Single;
    mv_l: Single;
    mv_x: Single;
    mv_y: Single;
    mv_dy: Single;
    mv_dx: Single;

    treb: Single;
    mid: Single;
    bass: Single;
    bass_old: Single;
    beat_sensitivity: Single;
    treb_att: Single;
    mid_att: Single;
    bass_att: Single;
    progress: Single;
    frame: Integer;

    { PER_FRAME CONSTANTS END }

    { PER_PIXEL CONSTANTS BEGIN }

    x_per_pixel: Single;
    y_per_pixel: Single;
    rad_per_pixel: Single;
    ang_per_pixel: Single;

    { PER_PIXEL CONSTANT END }


    fRating: Single;
    fGammaAdj: Single;
    fVideoEchoZoom: Single;
    fVideoEchoAlpha: Single;

    nVideoEchoOrientation: Integer;
    nWaveMode: Integer;
    bAdditiveWaves: Integer;
    bWaveDots: Integer;
    bWaveThick: Integer;
    bModWaveAlphaByVolume: Integer;
    bMaximizeWaveColor: Integer;
    bTexWrap: Integer;
    bDarkenCenter: Integer;
    bRedBlueStereo: Integer;
    bBrighten: Integer;
    bDarken: Integer;
    bSolarize: Integer;
    bInvert: Integer;
    bMotionVectorsOn: Integer;
    fps: Integer;

    fWaveAlpha: Single;
    fWaveScale: Single;
    fWaveSmoothing: Single;
    fWaveParam: Single;
    fModWaveAlphaStart: Single;
    fModWaveAlphaEnd: Single;
    fWarpAnimSpeed: Single;
    fWarpScale: Single;
    fShader: Single;

    
    { Q VARIABLES START }

    q1: Single;
    q2: Single;
    q3: Single;
    q4: Single;
    q5: Single;
    q6: Single;
    q7: Single;
    q8: Single;


    { Q VARIABLES END }

    zoom_mesh: PPSingle;
    zoomexp_mesh: PPSingle;
    rot_mesh: PPSingle;

    sx_mesh: PPSingle;
    sy_mesh: PPSingle;
    dx_mesh: PPSingle;
    dy_mesh: PPSingle;
    cx_mesh: PPSingle;
    cy_mesh: PPSingle;

    x_mesh: PPSingle;
    y_mesh: PPSingle;
    rad_mesh: PPSingle;
    theta_mesh: PPSingle;
  end;

  PProjectMState = ^TProjectMState;
  TProjectMState = record
    fontURLStr: string;
    presetURLStr: string;
    titleStr: string;
    pm: _TProjectM;
  end;

{ projectM.h declarations }
procedure _projectM_init(pm: _PProjectM); cdecl; external libprojectM name 'projectM_init';
procedure _projectM_reset(pm: _PProjectM); cdecl; external libprojectM name 'projectM_reset';
procedure _projectM_resetGL(pm: _PProjectM; width: Integer; height: Integer); cdecl; external libprojectM name 'projectM_resetGL';
procedure _projectM_setTitle(pm: _PProjectM; title: PChar); cdecl; external libprojectM name 'projectM_setTitle';
procedure _renderFrame(pm: _PProjectM); cdecl; external libprojectM name 'renderFrame';

{ PCM.h declarations }
procedure _addPCMfloat(pcm_data: PSingle; samples: integer); cdecl; external libprojectM name 'addPCMfloat';
procedure _addPCM16(pcm_data: PPCM16); cdecl; external libprojectM name 'addPCM16';
procedure _addPCM16Data(pcm_data: PSmallint; samples: Smallint); cdecl; external libprojectM name 'addPCM16Data';
procedure _addPCM8_512(pcm_data: PPCM8_512); cdecl; external libprojectM name 'addPCM8';

{ console_interface.h declarations }
procedure _key_handler(pm: _PProjectM;
                       event:    TProjectMEvent;
                       keycode:  TProjectMKeycode;
                       modifier: TProjectMModifier); cdecl; external libprojectM name 'key_handler';




{**************** EXTERNAL SECTION ****************}


constructor TProjectM.Create(gx, gy: integer; fps: integer;
  texsize: integer; width, height: integer;
  const presetsDir, fontsDir: string;
  const titleFont, menuFont: string);
var
  state: PProjectMState;
begin
  New(state);
  data := state;

  with state^ do
  begin
    // copy strings (Note: do not use e.g. PChar(presetsDir) directly, it might
    // be a pointer to local stack data that is invalid after the calling function returns)
    fontURLStr   := fontsDir;
    presetURLStr := presetsDir;
    
    _projectM_reset(@pm);

    pm.fullscreen := 0;
    pm.renderTarget^.texsize := texsize;
    pm.gx := gx;
    pm.gy := gy;
    pm.fps := fps;
    pm.renderTarget^.usePbuffers := 0;
    pm.fontURL   := PChar(fontURLStr);
    pm.presetURL := PChar(presetURLStr);

  	_projectM_init(@pm);
  end;
end;

procedure TProjectM.ResetGL(width, height: Integer);
begin
  _projectM_resetGL(@PProjectMState(data).pm, width, height);
end;

procedure TProjectM.SetTitle(const title: string);
var
  state: PProjectMState;
begin
  state := PProjectMState(data);
  with state^ do
  begin
    titleStr := title;
    pm.title := PChar(titleStr);
    pm.showtitle := 1;
  end;
end;

procedure TProjectM.RenderFrame();
begin
  _renderFrame(@PProjectMState(data).pm);
end;

procedure TProjectM.AddPCMfloat(pcmData: PSingle; samples: integer);
begin
  _addPCMfloat(pcmData, samples);
end;

procedure TProjectM.AddPCM16(pcmData: PPCM16);
begin
  _addPCM16(pcmData);
end;

procedure TProjectM.AddPCM16Data(pcmData: PSmallint; samples: Smallint);
begin
  _addPCM16Data(pcmData, samples);
end;

procedure TProjectM.AddPCM8_512(pcmData: PPCM8_512);
begin
  _addPCM8_512(pcmData);
end;

procedure TProjectM.KeyHandler(event:    TProjectMEvent;
                               keycode:  TProjectMKeycode;
                               modifier: TProjectMModifier);
begin
  _key_handler(@PProjectMState(data).pm, event, keycode, modifier);
end;

procedure TProjectM.RandomPreset();
begin
  KeyHandler(PROJECTM_KEYDOWN, PROJECTM_K_r_LOWERCASE, PROJECTM_KMOD_LSHIFT);
end;

procedure TProjectM.PreviousPreset();
begin
  KeyHandler(PROJECTM_KEYDOWN, PROJECTM_K_p_LOWERCASE, PROJECTM_KMOD_LSHIFT);
end;

procedure TProjectM.NextPreset();
begin
  KeyHandler(PROJECTM_KEYDOWN, PROJECTM_K_n_LOWERCASE, PROJECTM_KMOD_LSHIFT);
end;

procedure TProjectM.ToggleShowPresetNames();
begin
  KeyHandler(PROJECTM_KEYDOWN, PROJECTM_K_F3, PROJECTM_KMOD_LSHIFT);
end;

destructor TProjectM.Destroy();
begin
  Dispose(PProjectMState(data));
  data := nil;
end;

