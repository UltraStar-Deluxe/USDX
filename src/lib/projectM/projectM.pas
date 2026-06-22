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
  ctypes;

const
  projectM__lib = 'projectM-4';

type
  projectm_log_level = (
    PROJECTM_LOG_LEVEL_NOTSET = 0, //!< No specific log level, use default (INFO).
    PROJECTM_LOG_LEVEL_TRACE = 1,  //!< Verbose trace logging. Only enabled in debug builds by default.
    PROJECTM_LOG_LEVEL_DEBUG = 2,  //!< Development-related debug logging. Only enabled in debug builds by default.
    PROJECTM_LOG_LEVEL_INFO = 3,   //!< Informational messages.
    PROJECTM_LOG_LEVEL_WARN = 4,   //!< Warnings about non-critical issues.
    PROJECTM_LOG_LEVEL_ERROR = 5,  //!< Recoverable errors, e.g. shader compilation or I/O errors.
    PROJECTM_LOG_LEVEL_FATAL = 6   //!< Irrecoverable errors preventing projectM from working.
  );
  projectm_channels = (
    PROJECTM_MONO = 1,
    PROJECTM_STEREO = 2
  );
  projectm_handle = pointer;
  projectm_load_proc = procedure(name: PAnsiChar; user_data: pointer); cdecl;
  projectm_preset_switch_requested_event = procedure(is_hard_cut: cbool; user_data: pointer); cdecl;
  projectm_log_callback = procedure(message: PAnsiChar; log_level: projectm_log_level; user_data: pointer); cdecl;

function projectm_create(): projectm_handle; cdecl; external projectM__lib;
function projectm_create_with_opengl_load_proc(load_proc: projectm_load_proc; user_data: pointer): projectm_handle; cdecl; external projectM__lib;
procedure projectm_destroy(instance: projectm_handle); cdecl; external projectM__lib;
procedure projectm_load_preset_file(instance: projectm_handle; filename: PAnsiChar; smooth_transition: cbool); cdecl; external projectM__lib;
procedure projectm_load_preset_data(nstance: projectm_handle; data: PAnsiChar; smooth_transition: cbool); cdecl; external projectM__lib;
procedure projectm_set_preset_switch_requested_event_callback(instance:projectm_handle; callback: projectm_preset_switch_requested_event; user_data: pointer); cdecl; external projectM__lib;
procedure projectm_opengl_render_frame(instance: projectm_handle); cdecl; external projectM__lib;
procedure projectm_pcm_add_float(instance: projectm_handle; samples: pcfloat; count: cuint; channels: projectm_channels); cdecl; external projectM__lib;
procedure projectm_set_mesh_size(instance: projectm_handle; width, height: csize_t); cdecl; external projectM__lib;
procedure projectm_set_fps(instance: projectm_handle; fps: cint32); cdecl; external projectM__lib;
procedure projectm_set_window_size(instance: projectm_handle; width, height: csize_t); cdecl; external projectM__lib;
procedure projectm_set_soft_cut_duration(instance: projectm_handle; seconds: cdouble); cdecl; external projectM__lib;
procedure projectm_set_preset_duration(instance: projectm_handle; seconds: cdouble); cdecl; external projectM__lib;
procedure projectm_set_easter_egg(instance: projectm_handle; value: cfloat); cdecl; external projectM__lib;
procedure projectm_set_hard_cut_sensitivity(instance: projectm_handle; sensitivity: cfloat); cdecl; external projectM__lib;
procedure projectm_set_aspect_correction(instance: projectm_handle; enabled: cbool); cdecl; external projectM__lib;

// The ProjectM logging interface is only available since v4.2.0
// Make sure to guard these calls with UseProjectMLogging definition
procedure projectm_set_log_callback(callback: projectm_log_callback; current_thread_only: cbool; user_data: pointer); cdecl; external projectM__lib;
procedure projectm_set_log_level(log_level: projectm_log_level; current_thread_only: cbool); cdecl; external projectM__lib;
implementation
end.
