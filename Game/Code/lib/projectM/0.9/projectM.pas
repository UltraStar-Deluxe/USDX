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
  SysUtils;

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
  // 16bit non-interleaved data
  TPCM16 = array[0..1, 0..511] of Smallint;
  PPCM16 = ^TPCM16;
  // 8bit non-interleaved data
  TPCM8_512 = array[0..1, 0..511] of byte;
  PPCM8_512 = ^TPCM8_512;

{ Event types }
type
  TProjectMEvent = integer;
const
  PROJECTM_KEYUP       = 0;
  PROJECTM_KEYDOWN     = 1;
  PROJECTM_VIDEORESIZE = 2;
  PROJECTM_VIDEOQUIT   = 3;
  PROJECTM_NONE        = 4;

{ Keycodes }
type
  TProjectMKeycode = integer;
const
    PROJECTM_K_RETURN    =  0;
    PROJECTM_K_RIGHT     =  1;
    PROJECTM_K_LEFT      =  2;
    PROJECTM_K_UP        =  3;
    PROJECTM_K_DOWN      =  4;
    PROJECTM_K_PAGEUP    =  5;
    PROJECTM_K_PAGEDOWN  =  6;
    PROJECTM_K_INSERT    =  7;
    PROJECTM_K_DELETE    =  8;
    PROJECTM_K_ESCAPE    =  9;
    PROJECTM_K_LSHIFT    = 10;
    PROJECTM_K_RSHIFT    = 11;
    PROJECTM_K_CAPSLOCK  = 12;
    PROJECTM_K_LCTRL     = 13;
    PROJECTM_K_HOME      = 14;
    PROJECTM_K_END       = 15;
    PROJECTM_K_BACKSPACE = 16;

    PROJECTM_K_F1        = 17;
    PROJECTM_K_F2        = (PROJECTM_K_F1 +  1);
    PROJECTM_K_F3        = (PROJECTM_K_F1 +  2);
    PROJECTM_K_F4        = (PROJECTM_K_F1 +  3);
    PROJECTM_K_F5        = (PROJECTM_K_F1 +  4);
    PROJECTM_K_F6        = (PROJECTM_K_F1 +  5);
    PROJECTM_K_F7        = (PROJECTM_K_F1 +  6);
    PROJECTM_K_F8        = (PROJECTM_K_F1 +  7);
    PROJECTM_K_F9        = (PROJECTM_K_F1 +  8);
    PROJECTM_K_F10       = (PROJECTM_K_F1 +  9);
    PROJECTM_K_F11       = (PROJECTM_K_F1 + 10);
    PROJECTM_K_F12       = (PROJECTM_K_F1 + 11);

    PROJECTM_K_0         = 48;
    PROJECTM_K_1         = (PROJECTM_K_0 + 1);
    PROJECTM_K_2         = (PROJECTM_K_0 + 2);
    PROJECTM_K_3         = (PROJECTM_K_0 + 3);
    PROJECTM_K_4         = (PROJECTM_K_0 + 4);
    PROJECTM_K_5         = (PROJECTM_K_0 + 5);
    PROJECTM_K_6         = (PROJECTM_K_0 + 6);
    PROJECTM_K_7         = (PROJECTM_K_0 + 7);
    PROJECTM_K_8         = (PROJECTM_K_0 + 8);
    PROJECTM_K_9         = (PROJECTM_K_0 + 9);

    { Upper case }
    PROJECTM_K_A_UPPERCASE = 65;
    PROJECTM_K_B_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  1);
    PROJECTM_K_C_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  2);
    PROJECTM_K_D_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  3);
    PROJECTM_K_E_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  4);
    PROJECTM_K_F_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  5);
    PROJECTM_K_G_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  6);
    PROJECTM_K_H_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  7);
    PROJECTM_K_I_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  8);
    PROJECTM_K_J_UPPERCASE = (PROJECTM_K_A_UPPERCASE +  9);
    PROJECTM_K_K_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 10);
    PROJECTM_K_L_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 11);
    PROJECTM_K_M_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 12);
    PROJECTM_K_N_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 13);
    PROJECTM_K_O_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 14);
    PROJECTM_K_P_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 15);
    PROJECTM_K_Q_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 16);
    PROJECTM_K_R_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 17);
    PROJECTM_K_S_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 18);
    PROJECTM_K_T_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 19);
    PROJECTM_K_U_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 20);
    PROJECTM_K_V_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 21);
    PROJECTM_K_W_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 22);
    PROJECTM_K_X_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 23);
    PROJECTM_K_Y_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 24);
    PROJECTM_K_Z_UPPERCASE = (PROJECTM_K_A_UPPERCASE + 25);

    { Lower case }
    PROJECTM_K_a_LOWERCASE = 97;
    PROJECTM_K_b_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  1);
    PROJECTM_K_c_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  2);
    PROJECTM_K_d_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  3);
    PROJECTM_K_e_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  4);
    PROJECTM_K_f_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  5);
    PROJECTM_K_g_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  6);
    PROJECTM_K_h_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  7);
    PROJECTM_K_i_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  8);
    PROJECTM_K_j_LOWERCASE = (PROJECTM_K_a_LOWERCASE +  9);
    PROJECTM_K_k_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 10);
    PROJECTM_K_l_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 11);
    PROJECTM_K_m_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 12);
    PROJECTM_K_n_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 13);
    PROJECTM_K_o_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 14);
    PROJECTM_K_p_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 15);
    PROJECTM_K_q_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 16);
    PROJECTM_K_r_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 17);
    PROJECTM_K_s_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 18);
    PROJECTM_K_t_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 19);
    PROJECTM_K_u_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 20);
    PROJECTM_K_v_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 21);
    PROJECTM_K_w_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 22);
    PROJECTM_K_x_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 23);
    PROJECTM_K_y_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 24);
    PROJECTM_K_z_LOWERCASE = (PROJECTM_K_a_LOWERCASE + 25);

    PROJECTM_K_NONE        = (PROJECTM_K_z_LOWERCASE + 1);

{ Modifiers }
type
  TProjectMModifier = integer;
const
    PROJECTM_KMOD_LSHIFT = 0;
    PROJECTM_KMOD_RSHIFT = 1;
    PROJECTM_KMOD_CAPS   = 2;
    PROJECTM_KMOD_LCTRL  = 3;
    PROJECTM_KMOD_RCTRL  = 4;

type  
  PProjectM = ^TProjectM;
  TProjectM = class(TObject)
    private
      _pm: Pointer;
    public
      constructor Create(gx, gy: integer; fps: integer;
        texsize: integer; width, height: integer;
        presetsDir, fontsDir: string);

      procedure ResetGL(width, height: Integer);
      procedure SetTitle(title: string);
      procedure RenderFrame();

      procedure AddPCMfloat(pcmData: PSingle; samples: integer);
      procedure AddPCM16(pcmData: PPCM16);
      procedure AddPCM16Data(pcmData: PSmallint; samples: Smallint);
      procedure AddPCM8_512(pcmData: PPCM8_512);

      procedure RandomPreset();
      procedure PreviousPreset();
      procedure NextPreset();
      procedure ToggleShowPresetNames();

      procedure KeyHandler(event:    TProjectMEvent;
                           keycode:  TProjectMKeycode;
                           modifier: TProjectMModifier);

      destructor Destroy(); override;
  end;

implementation

uses
{$IFNDEF win32}
  baseunix,
{$ENDIF}
  OpenGL12;


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
    {$ifdef MACOS}
      origContext: Pointer;
      pbufferContext: Pointer;
      pbuffer: Pointer;
    {$endif}

  { Render target texture ID for non-pbuffer systems }
    textureID: array[0..2] of TGLuint;
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

    {$ifndef Win32}
    { The first ticks value of the application }
    startTime: timeval;
    {$else}
    startTime: Longint;
    {$endif Win32}
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
  presetsDir, fontsDir: string);
var
  pm: _PProjectM;
begin
  New(pm);
  _pm := pm;
	_projectM_reset(pm);

  pm^.fullscreen := 0;
  pm^.renderTarget^.texsize := texsize;
  pm^.gx := gx;
  pm^.gy := gy;
  pm^.fps := fps;
  pm^.renderTarget^.usePbuffers := 0;

  pm^.fontURL   := PChar(fontsDir);
  pm^.presetURL := PChar(presetsDir);

	_projectM_init(pm);
end;

procedure TProjectM.ResetGL(width, height: Integer);
begin
  _projectM_resetGL(_pm, width, height);
end;

procedure TProjectM.SetTitle(title: string);
var pm: _PProjectM;
begin
  pm := _pm;
  pm^.title := PChar(title);
  pm^.showtitle := 1;
end;

procedure TProjectM.RenderFrame();
begin
  _renderFrame(_pm);
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
  _key_handler(_pm, event, keycode, modifier);
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
  Dispose(_pm);
  _pm := nil;
end;

end.
