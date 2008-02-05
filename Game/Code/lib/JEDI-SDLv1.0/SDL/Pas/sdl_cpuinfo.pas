unit sdl_cpuinfo;
{
  $Id: sdl_cpuinfo.pas,v 1.2 2004/02/18 22:52:53 savage Exp $

}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{       Conversion of the Simple DirectMedia Layer Headers                     }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997-2004  Sam Lantinga                                        }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_cpuinfo.h                                       }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2004 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{
  $Log: sdl_cpuinfo.pas,v $
  Revision 1.2  2004/02/18 22:52:53  savage
  Forgot to add jedi-sdl.inc file. It's there now.

  Revision 1.1  2004/02/18 22:35:54  savage
  Brought sdl.pas up to 1.2.7 compatability
  Thus...
  Added SDL_GL_STEREO,
      SDL_GL_MULTISAMPLEBUFFERS,
      SDL_GL_MULTISAMPLESAMPLES

  Add DLL/Shared object functions
  function SDL_LoadObject( const sofile : PChar ) : Pointer;

  function SDL_LoadFunction( handle : Pointer; const name : PChar ) : Pointer;

  procedure SDL_UnloadObject( handle : Pointer );

  Added function to create RWops from const memory: SDL_RWFromConstMem()
  function SDL_RWFromConstMem(const mem: Pointer; size: Integer) : PSDL_RWops;

  Ported SDL_cpuinfo.h so Now you can test for Specific CPU types.


}
{******************************************************************************}

interface

{$I jedi-sdl.inc}

uses
  sdl;

{* This function returns true if the CPU has the RDTSC instruction
 *}
function SDL_HasRDTSC : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasRDTSC'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasRDTSC}

{* This function returns true if the CPU has MMX features
 *}
function SDL_HasMMX : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasMMX'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasMMX}

{* This function returns true if the CPU has MMX Ext. features
 *}
function SDL_HasMMXExt : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasMMXExt'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasMMXExt}

{* This function returns true if the CPU has 3DNow features
 *}
function SDL_Has3DNow : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_Has3DNow'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Has3DNow}

{* This function returns true if the CPU has 3DNow! Ext. features
 *}
function SDL_Has3DNowExt : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_Has3DNowExt'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Has3DNowExt}

{* This function returns true if the CPU has SSE features
 *}
function SDL_HasSSE : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasSSE'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasSSE}

{* This function returns true if the CPU has SSE2 features
 *}
function SDL_HasSSE2 : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasSSE2'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasSSE2}

{* This function returns true if the CPU has AltiVec features
 *}
function SDL_HasAltiVec : SDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_HasAltiVec'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_HasAltiVec}

implementation

end.
 