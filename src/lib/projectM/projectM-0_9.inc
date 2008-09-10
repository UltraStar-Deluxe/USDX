{$IFDEF Unix}
uses
  baseunix;
{$ENDIF}

const
{$IF Defined(MSWINDOWS)}
  libprojectM = 'libprojectM.dll';
{$ELSEIF Defined(UNIX)}
  libprojectM = 'libprojectM.so';
{$IFEND}

{**************** INTERNAL SECTION ****************}


type
  PPCfloat = ^PCfloat;

type
  _TContextType = cint;
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
    texsize: cint;

    { Application context }
    origContextType: _TContextType;

    usePbuffers: cint;

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

    hasInit: cint;

    noSwitch: cint;
    pcmframes: cint;
    freqframes: cint;
    totalframes: cint;

    showfps: cint;
    showtitle: cint;
    showpreset: cint;
    showhelp: cint;
    showstats: cint;

    studio: cint;

    fbuffer: PGLubyte;

    {$IFNDEF MSWINDOWS}
    { The first ticks value of the application }
    startTime: timeval;
    {$ELSE}
    startTime: clong;
    {$ENDIF}
    Time: cfloat;

    { Render target texture ID }
    renderTarget: _PRenderTarget;

    disp: array[0..79] of Char;

    wave_o: cfloat;

    //int texsize=1024;   //size of texture to do actual graphics
    fvw: cint;   //fullscreen dimensions
    fvh: cint;
    wvw: cint;   //windowed dimensions
    wvh: cint;
    vw: cint;    //runtime dimensions
    vh: cint;
    fullscreen: cint;

    maxsamples: cint;  //size of PCM buffer
    numsamples: cint;  //size of new PCM info
    pcmdataL: PCfloat;     //holder for most recent pcm data
    pcmdataR: PCfloat;     //holder for most recent pcm data

    avgtime: cint;     //# frames per preset
    
    title: PChar;
    drawtitle: cint;

    correction: cint;

    vol: cfloat;

    //per pixel equation variables
    gridx: PPCfloat;     //grid containing interpolated mesh
    gridy: PPCfloat;
    origtheta: PPCfloat; //grid containing interpolated mesh reference values
    origrad: PPCfloat;
    origx: PPCfloat;     //original mesh
    origy: PPCfloat;
    origx2: PPCfloat;    //original mesh
    origy2: PPCfloat;

    { Timing information }
    mspf: cint;
    timed: cint;
    timestart: cint;
    nohard: cint;
    count: cint;
    realfps,
    fpsstart: cfloat;

    { PCM data }
    vdataL: array[0..511] of cfloat;  //holders for FFT data (spectrum)
    vdataR: array[0..511] of cfloat;

    { Various toggles }
    doPerPixelEffects: cint;
    doIterative: cint;

    { ENGINE VARIABLES }
    { From engine_vars.h }
    preset_name: array[0..255] of Char;

    { PER FRAME CONSTANTS BEGIN }
    zoom: cfloat;
    zoomexp: cfloat;
    rot: cfloat;
    warp: cfloat;

    sx: cfloat;
    sy: cfloat;
    dx: cfloat;
    dy: cfloat;
    cx: cfloat;
    cy: cfloat;

    gy: cint;
    gx: cint;

    decay: cfloat;

    wave_r: cfloat;
    wave_g: cfloat;
    wave_b: cfloat;
    wave_x: cfloat;
    wave_y: cfloat;
    wave_mystery: cfloat;

    ob_size: cfloat;
    ob_r: cfloat;
    ob_g: cfloat;
    ob_b: cfloat;
    ob_a: cfloat;

    ib_size: cfloat;
    ib_r: cfloat;
    ib_g: cfloat;
    ib_b: cfloat;
    ib_a: cfloat;

    meshx: cint;
    meshy: cint;

    mv_a: cfloat;
    mv_r: cfloat;
    mv_g: cfloat;
    mv_b: cfloat;
    mv_l: cfloat;
    mv_x: cfloat;
    mv_y: cfloat;
    mv_dy: cfloat;
    mv_dx: cfloat;

    treb: cfloat;
    mid: cfloat;
    bass: cfloat;
    bass_old: cfloat;
    beat_sensitivity: cfloat;
    treb_att: cfloat;
    mid_att: cfloat;
    bass_att: cfloat;
    progress: cfloat;
    frame: cint;

    { PER_FRAME CONSTANTS END }

    { PER_PIXEL CONSTANTS BEGIN }

    x_per_pixel: cfloat;
    y_per_pixel: cfloat;
    rad_per_pixel: cfloat;
    ang_per_pixel: cfloat;

    { PER_PIXEL CONSTANT END }


    fRating: cfloat;
    fGammaAdj: cfloat;
    fVideoEchoZoom: cfloat;
    fVideoEchoAlpha: cfloat;

    nVideoEchoOrientation: cint;
    nWaveMode: cint;
    bAdditiveWaves: cint;
    bWaveDots: cint;
    bWaveThick: cint;
    bModWaveAlphaByVolume: cint;
    bMaximizeWaveColor: cint;
    bTexWrap: cint;
    bDarkenCenter: cint;
    bRedBlueStereo: cint;
    bBrighten: cint;
    bDarken: cint;
    bSolarize: cint;
    bInvert: cint;
    bMotionVectorsOn: cint;
    fps: cint;

    fWaveAlpha: cfloat;
    fWaveScale: cfloat;
    fWaveSmoothing: cfloat;
    fWaveParam: cfloat;
    fModWaveAlphaStart: cfloat;
    fModWaveAlphaEnd: cfloat;
    fWarpAnimSpeed: cfloat;
    fWarpScale: cfloat;
    fShader: cfloat;

    
    { Q VARIABLES START }

    q1: cfloat;
    q2: cfloat;
    q3: cfloat;
    q4: cfloat;
    q5: cfloat;
    q6: cfloat;
    q7: cfloat;
    q8: cfloat;


    { Q VARIABLES END }

    zoom_mesh: PPCfloat;
    zoomexp_mesh: PPCfloat;
    rot_mesh: PPCfloat;

    sx_mesh: PPCfloat;
    sy_mesh: PPCfloat;
    dx_mesh: PPCfloat;
    dy_mesh: PPCfloat;
    cx_mesh: PPCfloat;
    cy_mesh: PPCfloat;

    x_mesh: PPCfloat;
    y_mesh: PPCfloat;
    rad_mesh: PPCfloat;
    theta_mesh: PPCfloat;
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
procedure _projectM_resetGL(pm: _PProjectM; width: cint; height: cint); cdecl; external libprojectM name 'projectM_resetGL';
procedure _projectM_setTitle(pm: _PProjectM; title: PChar); cdecl; external libprojectM name 'projectM_setTitle';
procedure _renderFrame(pm: _PProjectM); cdecl; external libprojectM name 'renderFrame';

{ PCM.h declarations }
procedure _addPCMfloat(pcm_data: PCfloat; samples: cint); cdecl; external libprojectM name 'addPCMfloat';
procedure _addPCM16(pcm_data: PPCM16); cdecl; external libprojectM name 'addPCM16';
procedure _addPCM16Data(pcm_data: PCshort; samples: cshort); cdecl; external libprojectM name 'addPCM16Data';
procedure _addPCM8_512(pcm_data: PPCM8_512); cdecl; external libprojectM name 'addPCM8';

{ console_interface.h declarations }
procedure _key_handler(pm: _PProjectM;
                       event:    TProjectMEvent;
                       keycode:  TProjectMKeycode;
                       modifier: TProjectMModifier); cdecl; external libprojectM name 'key_handler';




{**************** EXTERNAL SECTION ****************}


constructor TProjectM.Create(gx, gy: cint; fps: integer;
  texsize: integer; width, height: integer;
  const presetsDir, fontsDir: string;
  const titleFont, menuFont: string);
var
  state: PProjectMState;
begin
  inherited Create();

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

procedure TProjectM.ResetGL(width, height: integer);
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
  _addPCMfloat(PCfloat(pcmData), samples);
end;

procedure TProjectM.AddPCM16(pcmData: PPCM16);
begin
  _addPCM16(pcmData);
end;

procedure TProjectM.AddPCM16Data(pcmData: PSmallint; samples: Smallint);
begin
  _addPCM16Data(PCshort(pcmData), samples);
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
  inherited;
end;

