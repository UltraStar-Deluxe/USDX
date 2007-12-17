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
  OpenGL12;

const
{$IFDEF win32}
  libprojectM = 'projectM-cwrapper.dll';
{$ELSE}
  libprojectM = 'libprojectM-cwrapper.so';
{$ENDIF}

const
  PROJECTM_VERSION = '1.00.00';
  PROJECTM_TITLE   = 'projectM 1.00.00';

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
        presetsDir, fontsDir: string;
        titleFont: string = 'Vera.ttf';
        menuFont: string = 'Vera.ttf'); overload;
      constructor Create(configFile: string); overload;

      procedure ResetGL(width, height: Integer);
      procedure SetTitle(title: string);
      procedure RenderFrame();

      procedure AddPCMfloat(pcmData: PSingle; samples: integer);
      procedure AddPCM16(pcmData: PPCM16);
      procedure AddPCM16Data(pcmData: PSmallint; samples: Smallint);
      procedure AddPCM8_512(pcmData: PPCM8_512);
      procedure AddPCM8_1024(pcmData: PPCM8_1024);

      procedure RandomPreset();
      procedure PreviousPreset();
      procedure NextPreset();
      procedure ToggleShowPresetNames();

      function InitRenderToTexture(): GLuint;

      procedure KeyHandler(event:    TProjectMEvent;
                           keycode:  TProjectMKeycode;
                           modifier: TProjectMModifier);

      destructor Destroy();
  end;

implementation


{**************** INTERNAL SECTION ****************}


type
  _PProjectM = Pointer;

{ projectM.hpp declarations }
function _projectM_create1(config_file: PChar): _PProjectM; cdecl; external libprojectM name 'projectM_create1';
function _projectM_create2(gx: integer; gy: integer; fps: integer;
  texsize: integer; width: integer; height: integer;
  preset_url: PChar; title_fonturl: PChar; title_menuurl: PChar): _PProjectM; cdecl; external libprojectM name 'projectM_create2';

procedure _projectM_resetGL(pm: _PProjectM; width: integer; height: integer); cdecl; external libprojectM name 'projectM_resetGL';
procedure _projectM_setTitle(pm: _PProjectM; title: PChar); cdecl; external libprojectM name 'projectM_setTitle';
procedure _projectM_renderFrame(pm: _PProjectM); cdecl; external libprojectM name 'projectM_renderFrame';
function _projectM_initRenderToTexture(pm: _PProjectM): Cardinal; cdecl; external libprojectM name 'projectM_initRenderToTexture';

procedure _projectM_free(pm: _PProjectM); cdecl; external libprojectM name 'projectM_free';

procedure _projectM_key_handler(pm: _PProjectM; event: TProjectMEvent;
  keycode: TProjectMKeycode; modifier: TProjectMModifier); cdecl; external libprojectM name 'projectM_key_handler';

{ PCM.hpp declarations }
procedure _PCM_addPCMfloat(pm: _PProjectM; pcm_data: PSingle; samples: integer); cdecl; external libprojectM name 'PCM_addPCMfloat';
procedure _PCM_addPCM16(pm: _PProjectM; pcm_data: PPCM16); cdecl; external libprojectM name 'PCM_addPCM16';
procedure _PCM_addPCM16Data(pm: _PProjectM; pcm_data: PSmallint; samples: Smallint); cdecl; external libprojectM name 'PCM_addPCM16Data';
procedure _PCM_addPCM8_512(pm: _PProjectM; pcm_data: PPCM8_512); cdecl; external libprojectM name 'PCM_addPCM8_512';
procedure _PCM_addPCM8_1024(pm: _PProjectM; pcm_data: PPCM8_1024); cdecl; external libprojectM name 'PCM_addPCM8';


{**************** EXTERNAL SECTION ****************}

constructor TProjectM.Create(gx, gy: integer; fps: integer;
  texsize: integer; width, height: integer;
  presetsDir, fontsDir: string; titleFont, menuFont: string);
begin
  _pm := _projectM_create2(gx, gy, fps, texsize, width, height,
            PChar(presetsDir),
            PChar(fontsDir + '/' + titleFont),
            PChar(fontsDir + '/' + menuFont))
end;

constructor TProjectM.Create(configFile: string);
begin
  _pm := _projectM_create1(PChar(configFile));
end;

procedure TProjectM.ResetGL(width, height: Integer);
begin
  _projectM_resetGL(_pm, width, height);
end;

procedure TProjectM.SetTitle(title: string);
begin
  _projectM_setTitle(_pm, PChar(title));
end;

procedure TProjectM.RenderFrame();
begin
  _projectM_renderFrame(_pm);
end;

procedure TProjectM.AddPCMfloat(pcmData: PSingle; samples: integer);
begin
  _PCM_addPCMfloat(_pm, pcmData, samples);
end;

procedure TProjectM.AddPCM16(pcmData: PPCM16);
begin
  _PCM_addPCM16(_pm, pcmData);
end;

{**
 * Passes interleaved stereo PCM-samples to projectM.
 *}
procedure TProjectM.AddPCM16Data(pcmData: PSmallint; samples: Smallint);
begin
  _PCM_addPCM16Data(_pm, pcmData, samples);
end;

procedure TProjectM.AddPCM8_512(pcmData: PPCM8_512);
begin
  _PCM_addPCM8_512(_pm, pcmData);
end;

procedure TProjectM.AddPCM8_1024(pcmData: PPCM8_1024);
begin
  _PCM_addPCM8_1024(_pm, pcmData);
end;

{**
 * If the result is > -1 projectM will render to a texture.
 * The texture-ID is the return-value.
 *}
function TProjectM.InitRenderToTexture(): GLuint;
begin
  result := _projectM_initRenderToTexture(_pm);
end;

procedure TProjectM.KeyHandler(event:    TProjectMEvent;
                               keycode:  TProjectMKeycode;
                               modifier: TProjectMModifier);
begin
  _projectM_key_handler(_pm, event, keycode, modifier);
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
  _projectM_free(_pm);
  _pm := nil;
end;

end.
