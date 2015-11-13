//uses

(**
 * Note: be careful with ProjectM's versioning scheme.
 *
 * Version  | Version in pkg-config .pc file
 * ---------+--------------------------------------------
 * 1.00     | 1.00
 * 1.01     | 1.00
 * 1.1      | 1.10
 * 1.2      | 1.2 (= 1.02)
 *
 * So the version number of 1.1 is bigger than that of 1.2.
 *)

const
{$IFDEF MSWINDOWS}
  // Note: static linking is not possible with delphi because it does neither
  // accept gcc nor MSVC object files (only Intel-style ones).
  libprojectM_cwrapper = 'projectM-cwrapper.dll';
{$ELSE}
  // static libs are not supported in the "external"-clause
  libprojectM_cwrapper = '';
  // statically link the cwrapper and dynamically link projectM
  {$L 'cwrapper/libprojectM-cwrapper.a'}
  {$LINKLIB projectM}
{$ENDIF}

{**************** INTERNAL SECTION ****************}


type
  _PProjectM = Pointer;

{ projectM.hpp declarations }
function _projectM_create1(config_file: PChar): _PProjectM; cdecl; external libprojectM_cwrapper name 'projectM_create1';
{$IF PROJECTM_VERSION < 1000000} // 0.9x
function _projectM_create2(gx: cint; gy: cint; fps: cint;
  texsize: cint; width: cint; height: cint;
  preset_url: PChar; title_fonturl: PChar; title_menuurl: PChar): _PProjectM; cdecl; external libprojectM_cwrapper name 'projectM_create2';
{$IFEND}

procedure _projectM_resetGL(pm: _PProjectM; width: cint; height: cint); cdecl; external libprojectM_cwrapper name 'projectM_resetGL';
procedure _projectM_setTitle(pm: _PProjectM; title: PChar); cdecl; external libprojectM_cwrapper name 'projectM_setTitle';
procedure _projectM_renderFrame(pm: _PProjectM); cdecl; external libprojectM_cwrapper name 'projectM_renderFrame';
function _projectM_initRenderToTexture(pm: _PProjectM): cuint; cdecl; external libprojectM_cwrapper name 'projectM_initRenderToTexture';

procedure _projectM_free(pm: _PProjectM); cdecl; external libprojectM_cwrapper name 'projectM_free';

procedure _projectM_key_handler(pm: _PProjectM; event: TProjectMEvent;
  keycode: TProjectMKeycode; modifier: TProjectMModifier); cdecl; external libprojectM_cwrapper name 'projectM_key_handler';

{$IF PROJECTM_VERSION > 1000000} // > 1.01
procedure _projectM_settings(pm: _PProjectM; settings: PSettings); cdecl; external libprojectM_cwrapper name 'projectM_settings';
{$IFEND}

{ PCM.hpp declarations }
procedure _PCM_addPCMfloat(pm: _PProjectM; pcm_data: PSingle; samples: cint); cdecl; external libprojectM_cwrapper name 'PCM_addPCMfloat';
procedure _PCM_addPCM16(pm: _PProjectM; pcm_data: PPCM16); cdecl; external libprojectM_cwrapper name 'PCM_addPCM16';
procedure _PCM_addPCM16Data(pm: _PProjectM; pcm_data: PCshort; samples: cshort); cdecl; external libprojectM_cwrapper name 'PCM_addPCM16Data';
procedure _PCM_addPCM8_512(pm: _PProjectM; pcm_data: PPCM8_512); cdecl; external libprojectM_cwrapper name 'PCM_addPCM8_512';
procedure _PCM_addPCM8_1024(pm: _PProjectM; pcm_data: PPCM8_1024); cdecl; external libprojectM_cwrapper name 'PCM_addPCM8';


{**************** EXTERNAL SECTION ****************}

// This constructor is present in projectM 1.0(1) but does not work with
// linux because of a bug.
(*
constructor TProjectM.Create(gx, gy: integer; fps: integer;
  texsize: integer; width, height: integer;
  const presetsDir, fontsDir: string;
  const titleFont, menuFont: string);
begin
  data := _projectM_create2(gx, gy, fps, texsize, width, height,
            PChar(presetsDir),
            PChar(fontsDir + PathDelim + titleFont),
            PChar(fontsDir + PathDelim + menuFont));
end;
*)

constructor TProjectM.Create(const configFile: string);
begin
  inherited Create();

  // we cannot catch C++ exceptions in delphi, so we have to check
  // if configFile is valid first
  if (not FileExists(configFile)) then
    raise Exception.Create('Invalid file: ' + configFile);

  data := _projectM_create1(PChar(configFile));
  if (data = nil) then
    raise Exception.Create('Creation of projectM object failed');
end;

procedure TProjectM.ResetGL(width, height: Integer);
begin
  _projectM_resetGL(data, width, height);
end;

procedure TProjectM.SetTitle(const title: string);
begin
  _projectM_setTitle(data, PChar(title));
end;

procedure TProjectM.RenderFrame();
begin
  _projectM_renderFrame(data);
end;

procedure TProjectM.AddPCMfloat(pcmData: PSingle; samples: integer);
begin
  _PCM_addPCMfloat(data, pcmData, samples);
end;

procedure TProjectM.AddPCM16(pcmData: PPCM16);
begin
  _PCM_addPCM16(data, pcmData);
end;

{**
 * Passes interleaved stereo PCM-samples to projectM.
 *}
procedure TProjectM.AddPCM16Data(pcmData: PSmallint; samples: Smallint);
begin
  _PCM_addPCM16Data(data, PCshort(pcmData), samples);
end;

procedure TProjectM.AddPCM8_512(pcmData: PPCM8_512);
begin
  _PCM_addPCM8_512(data, pcmData);
end;

procedure TProjectM.AddPCM8_1024(pcmData: PPCM8_1024);
begin
  _PCM_addPCM8_1024(data, pcmData);
end;

{**
 * If the result is > -1 projectM will render to a texture.
 * The texture-ID is the return-value.
 *}
function TProjectM.InitRenderToTexture(): GLuint;
begin
  result := _projectM_initRenderToTexture(data);
end;

procedure TProjectM.KeyHandler(event:    TProjectMEvent;
                               keycode:  TProjectMKeycode;
                               modifier: TProjectMModifier);
begin
  _projectM_key_handler(data, event, keycode, modifier);
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
                       
{$IF PROJECTM_VERSION > 1000000} // > 1.01
procedure TProjectM.Settings(var settings: TSettings);
begin
  _projectM_settings(data, @settings);
end;
{$IFEND}

destructor TProjectM.Destroy();
begin
  _projectM_free(data);
  data := nil;
  inherited;
end;

