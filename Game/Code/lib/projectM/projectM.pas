unit projectM;

{$IFDEF FPC}
  {$IFNDEF win32}
  {$LINKLIB libprojectM}
  {$ENDIF}
  {$MODE DELPHI}
  {$PACKENUM 4}
  {$PACKRECORDS C}
{$ENDIF}

interface

uses
{$IFNDEF win32}
  baseunix,
{$ENDIF}
  OpenGL12;

const
{$IFDEF win32}
  libprojectM = 'libprojectM.dll';
{$ELSE}
  libprojectM = 'libprojectM.so';
{$ENDIF}

const
  PROJECTM_VERSION = '0.99';
  PROJECTM_TITLE   = 'projectM 0.99';

type
  FLOAT = Single;
  PFLOAT = ^FLOAT;
  PPFLOAT = ^PFLOAT;
  INT = Integer;
  SHORT = Smallint;
  LONG = Longint;

type
  // 16bit interleaved data
  TPCM16Data = array[0..511, 0..1] of SHORT;
  PPCM16Data = ^TPCM16Data;
  // 16bit non-interleaved data
  TPCM16 = array[0..1, 0..511] of SHORT;
  PPCM16 = ^TPCM16;
  // 8bit non-interleaved data
  TPCM8  = array[0..1, 0..511] of byte;
  PPCM8  = ^TPCM8;

type
  TTextureScale = INT;
const
  SCALE_NEAREST   = 0;
  SCALE_MAGNIFY   = 1;
  SCALE_MINIFY    = 2;

type
  TContextType = INT;
const
  AGL_CONTEXT   = 0;
  CGL_CONTEXT   = 1;
  NSGL_CONTEXT  = 2;
  GLX_CONTEXT   = 3;
  WGL_CONTEXT   = 4;

type
  TPBufferPass = INT;
const  
  PBUFFER_PASS1 = 0;
  PBUFFER_PASS2 = 1;

type
  PRenderTarget = ^TRenderTarget;
  TRenderTarget = record
    { Texture size }
    texsize: INT;

    { Application context }
    origContextType: TContextType;

    usePbuffers: INT;

    {$ifdef LINUX}
      lock_func: procedure(); cdecl;
      unlock_func: procedure(); cdecl;
    {$endif}

    { Opaque pbuffer context and pbuffer }
    {$ifdef MACOS}
      origContext: Pointer;
      pbufferContext: Pointer;
      pbuffer: Pointer;
    {$endif}

  { Render target texture ID for non-pbuffer systems }
    textureID: array[0..2] of TGLuint;
  end;

  {
  procedure createPBuffers( width, height: INT; target: PRenderTarget );
  procedure lockPBuffer( target: PRenderTarget; pass: PBufferPass );
  procedure unlockPBuffer( target: PRenderTarget );
  function nearestPower2( value: INT; scaleRule: TTextureScale ): INT;
  }

  PProjectM = ^TProjectM;
  TProjectM = record
    presetURL: PChar;
    presetName: PChar;
    fontURL: PChar;

    hasInit: INT;

    noSwitch: INT;
    pcmframes: INT;
    freqframes: INT;
    totalframes: INT;

    showfps: INT;
    showtitle: INT;
    showpreset: INT;
    showhelp: INT;
    showstats: INT;

    studio: INT;

    fbuffer: PGLubyte;

    {$ifndef Win32}
    { The first ticks value of the application }
    startTime: timeval;
    {$else}
    startTime: LONG;
    {$endif Win32}
    Time: FLOAT;

    { Render target texture ID }
    renderTarget: PRenderTarget;

    disp: array[0..79] of Char;

    wave_o: FLOAT;

    //int texsize=1024;   //size of texture to do actual graphics
    fvw: INT;   //fullscreen dimensions
    fvh: INT;
    wvw: INT;   //windowed dimensions
    wvh: INT;
    vw: INT;    //runtime dimensions
    vh: INT;
    fullscreen: INT;

    maxsamples: INT;  //size of PCM buffer
    numsamples: INT;  //size of new PCM info
    pcmdataL: PFLOAT;     //holder for most recent pcm data
    pcmdataR: PFLOAT;     //holder for most recent pcm data

    avgtime: INT;     //# frames per preset
    
    title: PChar;
    drawtitle: INT;

    correction: INT;

    vol: FLOAT;

    //per pixel equation variables
    gridx: PPFLOAT;     //grid containing interpolated mesh
    gridy: PPFLOAT;
    origtheta: PPFLOAT; //grid containing interpolated mesh reference values
    origrad: PPFLOAT;
    origx: PPFLOAT;     //original mesh
    origy: PPFLOAT;
    origx2: PPFLOAT;    //original mesh
    origy2: PPFLOAT;

    { Timing information }
    mspf: INT;
    timed: INT;
    timestart: INT;
    nohard: INT;
    count: INT;
    realfps,
      fpsstart: FLOAT;

    { PCM data }
    vdataL: array[0..511] of FLOAT;  //holders for FFT data (spectrum)
    vdataR: array[0..511] of FLOAT;

    { Various toggles }
    doPerPixelEffects: INT;
    doIterative: INT;

    { ENGINE VARIABLES }
    { From engine_vars.h }
    preset_name: array[0..255] of Char;

    { PER FRAME CONSTANTS BEGIN }
    zoom: FLOAT;
    zoomexp: FLOAT;
    rot: FLOAT;
    warp: FLOAT;

    sx: FLOAT;
    sy: FLOAT;
    dx: FLOAT;
    dy: FLOAT;
    cx: FLOAT;
    cy: FLOAT;

    gy: INT;
    gx: INT;

    decay: FLOAT;

    wave_r: FLOAT;
    wave_g: FLOAT;
    wave_b: FLOAT;
    wave_x: FLOAT;
    wave_y: FLOAT;
    wave_mystery: FLOAT;

    ob_size: FLOAT;
    ob_r: FLOAT;
    ob_g: FLOAT;
    ob_b: FLOAT;
    ob_a: FLOAT;

    ib_size: FLOAT;
    ib_r: FLOAT;
    ib_g: FLOAT;
    ib_b: FLOAT;
    ib_a: FLOAT;

    meshx: INT;
    meshy: INT;

    mv_a: FLOAT;
    mv_r: FLOAT;
    mv_g: FLOAT;
    mv_b: FLOAT;
    mv_l: FLOAT;
    mv_x: FLOAT;
    mv_y: FLOAT;
    mv_dy: FLOAT;
    mv_dx: FLOAT;

    treb: FLOAT;
    mid: FLOAT;
    bass: FLOAT;
    bass_old: FLOAT;
    beat_sensitivity: FLOAT;
    treb_att: FLOAT;
    mid_att: FLOAT;
    bass_att: FLOAT;
    progress: FLOAT;
    frame: INT;

    { PER_FRAME CONSTANTS END }

    { PER_PIXEL CONSTANTS BEGIN }

    x_per_pixel: FLOAT;
    y_per_pixel: FLOAT;
    rad_per_pixel: FLOAT;
    ang_per_pixel: FLOAT;

    { PER_PIXEL CONSTANT END }


    fRating: FLOAT;
    fGammaAdj: FLOAT;
    fVideoEchoZoom: FLOAT;
    fVideoEchoAlpha: FLOAT;

    nVideoEchoOrientation: INT;
    nWaveMode: INT;
    bAdditiveWaves: INT;
    bWaveDots: INT;
    bWaveThick: INT;
    bModWaveAlphaByVolume: INT;
    bMaximizeWaveColor: INT;
    bTexWrap: INT;
    bDarkenCenter: INT;
    bRedBlueStereo: INT;
    bBrighten: INT;
    bDarken: INT;
    bSolarize: INT;
    bInvert: INT;
    bMotionVectorsOn: INT;
    fps: INT;

    fWaveAlpha: FLOAT;
    fWaveScale: FLOAT;
    fWaveSmoothing: FLOAT;
    fWaveParam: FLOAT;
    fModWaveAlphaStart: FLOAT;
    fModWaveAlphaEnd: FLOAT;
    fWarpAnimSpeed: FLOAT;
    fWarpScale: FLOAT;
    fShader: FLOAT;

    
    { Q VARIABLES START }

    q1: FLOAT;
    q2: FLOAT;
    q3: FLOAT;
    q4: FLOAT;
    q5: FLOAT;
    q6: FLOAT;
    q7: FLOAT;
    q8: FLOAT;


    { Q VARIABLES END }

    zoom_mesh: PPFLOAT;
    zoomexp_mesh: PPFLOAT;
    rot_mesh: PPFLOAT;

    sx_mesh: PPFLOAT;
    sy_mesh: PPFLOAT;
    dx_mesh: PPFLOAT;
    dy_mesh: PPFLOAT;
    cx_mesh: PPFLOAT;
    cy_mesh: PPFLOAT;

    x_mesh: PPFLOAT;
    y_mesh: PPFLOAT;
    rad_mesh: PPFLOAT;
    theta_mesh: PPFLOAT;
  end;

  { Functions }
  procedure projectM_init(pm: PProjectM); cdecl; external libprojectM;
  procedure projectM_reset(pm: PProjectM); cdecl; external libprojectM;
  procedure projectM_resetGL(pm: PProjectM; width: INT; height: INT); cdecl; external libprojectM;
  procedure projectM_setTitle(pm: PProjectM; title: PChar); cdecl; external libprojectM;
  procedure renderFrame(pm: PProjectM); cdecl; external libprojectM;

  {
  procedure draw_help(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_fps(pm: PProjectM; fps: Single); cdecl; external libprojectM;
  procedure draw_preset(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_title(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_stats(pm: PProjectM); cdecl; external libprojectM;

  procedure modulate_opacity_by_volume(pm: PProjectM); cdecl; external libprojectM;
  procedure maximize_colors(pm: PProjectM); cdecl; external libprojectM;
  procedure do_per_pixel_math(pm: PProjectM); cdecl; external libprojectM;
  procedure do_per_frame(pm: PProjectM); cdecl; external libprojectM;
  procedure darken_center(pm: PProjectM); cdecl; external libprojectM;

  procedure render_interpolation(pm: PProjectM); cdecl; external libprojectM;
  procedure render_texture_to_screen(pm: PProjectM); cdecl; external libprojectM;
  procedure render_texture_to_studio(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_motion_vectors(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_borders(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_shapes(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_waveform(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_custom_waves(pm: PProjectM); cdecl; external libprojectM;

  procedure draw_title_to_screen(pm: PProjectM); cdecl; external libprojectM;
  procedure draw_title_to_texture(pm: PProjectM); cdecl; external libprojectM;
  procedure get_title(pm: PProjectM); cdecl; external libprojectM;

  procedure reset_per_pixel_matrices(pm: PProjectM); cdecl; external libprojectM;
  procedure init_per_pixel_matrices(pm: PProjectM); cdecl; external libprojectM;
  procedure rescale_per_pixel_matrices(pm: PProjectM); cdecl; external libprojectM;
  }
  
  { PCM.h declarations }
  procedure addPCMfloat(pcm_data: PSingle; samples: integer); cdecl; external libprojectM;
  procedure addPCM16(pcm_data: PPCM16); cdecl; external libprojectM;
  procedure addPCM16Data(pcm_data: PPCM16Data; samples: Smallint); cdecl; external libprojectM;
  procedure addPCM8(pcm_data: PPCM8); cdecl; external libprojectM;

implementation

end.
