unit projectM;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}            (* use long strings *)
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  SysUtils,
  ctypes,
  gl,
  UConfig;

type
  // 16bit non-interleaved data
  TPCM16 = array[0..1, 0..511] of Smallint;
  PPCM16 = ^TPCM16;
  // 8bit non-interleaved data (512 samples)
  TPCM8_512 = array[0..1, 0..511] of byte;
  PPCM8_512 = ^TPCM8_512;
  // 8bit non-interleaved data (1024 samples)
  TPCM8_1024 = array[0..1, 0..1023] of byte;
  PPCM8_1024 = ^TPCM8_512;

{ Event types }
type
  TProjectMEvent = cint;
const
  PROJECTM_KEYUP       = 0;
  PROJECTM_KEYDOWN     = 1;
  PROJECTM_VIDEORESIZE = 2;
  PROJECTM_VIDEOQUIT   = 3;
  PROJECTM_NONE        = 4;

{ Keycodes }
type
  TProjectMKeycode = cint;
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
  TProjectMModifier = cint;
const
  PROJECTM_KMOD_LSHIFT = 0;
  PROJECTM_KMOD_RSHIFT = 1;
  PROJECTM_KMOD_CAPS   = 2;
  PROJECTM_KMOD_LCTRL  = 3;
  PROJECTM_KMOD_RCTRL  = 4;

type
  PSettings = ^TSettings;
  TSettings = record
    meshX: cint;
    meshY: cint;
    fps: cint;
    textureSize: cint;
    windowWidth: cint;
    windowHeight: cint;
    presetURL: PChar;
    titleFontURL: PChar;
    menuFontURL: PChar;
    smoothPresetDuration: cint;
    presetDuration: cint;
    beatSensitivity: cfloat;
    aspectCorrection: byte;
    easterEgg: cfloat;
    shuffleEnabled: byte;
  end;

type
  PProjectM = ^TProjectM;
  TProjectM = class(TObject)
    private
      data: Pointer;
    public
      {$IF PROJECTM_VERSION < 1000000} // 0.9x
      constructor Create(gx, gy: integer; fps: integer;
        texsize: integer; width, height: integer;
        const presetsDir, fontsDir: string;
        const titleFont: string = 'Vera.ttf';
        const menuFont: string = 'Vera.ttf'); overload;
      {$IFEND}
      {$IF PROJECTM_VERSION >= 1000000}
      constructor Create(const configFile: string); overload;
      {$IFEND}

      procedure ResetGL(width, height: Integer);
      procedure SetTitle(const title: string);
      procedure RenderFrame();

      procedure AddPCMfloat(pcmData: PSingle; samples: integer);
      procedure AddPCM16(pcmData: PPCM16);
      procedure AddPCM16Data(pcmData: PSmallint; samples: Smallint);
      procedure AddPCM8_512(pcmData: PPCM8_512);
      {$IF PROJECTM_VERSION >= 1000000}
      procedure AddPCM8_1024(pcmData: PPCM8_1024);
      {$IFEND}

      procedure RandomPreset();
      procedure PreviousPreset();
      procedure NextPreset();
      procedure ToggleShowPresetNames();

      {$IF PROJECTM_VERSION >= 1000000}
      function InitRenderToTexture(): GLuint;
      {$IFEND}

      procedure KeyHandler(event:    TProjectMEvent;
                           keycode:  TProjectMKeycode;
                           modifier: TProjectMModifier);

      {$IF PROJECTM_VERSION > 1000000} // > 1.01
      procedure Settings(var settings: TSettings);
      {$IFEND}

      destructor Destroy(); override;
  end;

implementation

{$IF PROJECTM_VERSION >= 1000000}
  {$I projectM-1_0.inc}
{$ELSE}
  {$I projectM-0_9.inc}
{$IFEND}

end.
