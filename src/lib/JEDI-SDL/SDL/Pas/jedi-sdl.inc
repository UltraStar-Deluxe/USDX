{
  $Id: jedi-sdl.inc,v 1.15 2007/05/29 21:30:48 savage Exp $
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{       Global Conditional Definitions for JEDI-SDL cross-compilation          }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{                                                                              }
{ Portions created by Prof. Abimbola Olowofoyeku are                           }
{ Copyright (C) 2000 - 2100 Prof. Abimbola Olowofoyeku.                        }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{  Dominqiue Louis <Dominique@SavageSoftware.com.au>                           }
{                                                                              }
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
{ This code has been copied from...                                            }
{ Global Conditional Definitions for Chief's UNZIP package                     }
{ By Prof. Abimbola Olowofoyeku (The African Chief)                            }
{          http://www.bigfoot.com/~African_Chief/                              }
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
{   2003-04-03   DL - Initial addition                                         }
{                                                                              }
{   2003-04-07   DL - Added Macro ON derective for FPC and OpenGL and removed  }
{                WEAKPACKAGE derective. WEAKPACKAGE should be set when         }
{                appropriate.                                                  }
{                                                                              }
{   2003-04-23 - DL : under instruction from Alexey Barkovoy I have added      }
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief) }
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   2004-01-19 - DL : Under instruction from Marco van de Voort, I have added  }
{                     Better FPC support for FreeBSD.                          }
{                                                                              }
(*
  $Log: jedi-sdl.inc,v $
  Revision 1.15  2007/05/29 21:30:48  savage
  Changes as suggested by Almindor for 64bit compatibility.

  Revision 1.14  2007/05/20 20:29:11  savage
  Initial Changes to Handle 64 Bits

  Revision 1.13  2007/01/21 15:51:45  savage
  Added Delphi 2006 support

  Revision 1.12  2006/11/19 18:41:01  savage
  removed THREADING ON flag as it is no longer needed in latest versions of FPC.

  Revision 1.11  2006/01/04 00:52:41  drellis
  Updated to include defined for ENDIAN values, SDL_BYTEORDER should now be correctly defined depending onthe platform. Code taken from sdl_mixer

  Revision 1.10  2005/05/22 18:42:31  savage
  Changes as suggested by Michalis Kamburelis. Thanks again.

  Revision 1.9  2004/12/23 23:42:17  savage
  Applied Patches supplied by Michalis Kamburelis ( THANKS! ), for greater FreePascal compatability.

  Revision 1.8  2004/10/20 22:43:04  savage
  Ensure that UNSAFE type warning are off in D9 as well

  Revision 1.7  2004/04/05 09:59:51  savage
  Changes for FreePacal as suggested by Marco

  Revision 1.6  2004/03/31 22:18:15  savage
  Small comment for turning off warning under GnuPascal

  Revision 1.5  2004/03/30 22:41:02  savage
  Added extra commenting due to previous compiler directive

  Revision 1.4  2004/03/30 22:08:33  savage
  Added Kylix Define

  Revision 1.3  2004/03/30 21:34:40  savage
  {$H+} needed for FPC compatiblity

  Revision 1.2  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

*)
{******************************************************************************}

{.$define Debug}           { uncomment for debugging }

{$IFNDEF FPC}
  {$IFDEF __GPC__}
    {$I-}
    {$W-} // turn off GPC warnings
    {$X+}
  {$ELSE} {__GPC__}
    {$IFDEF Debug}
      {$F+,D+,Q-,L+,R+,I-,S+,Y+,A+}
    {$ELSE}
      {$F+,Q-,R-,S-,I-,A+}
    {$ENDIF}
  {$ENDIF} {__GPC__}
{$ELSE}  {FPC}
  //{$M+}
{$ENDIF} {FPC}

{$IFDEF LINUX}
{$DEFINE UNIX}
{$ENDIF}

{$IFDEF ver70}
   {$IFDEF Windows}
     {$DEFINE Win16}
   {$ENDIF Windows}
   {$IFDEF MSDOS}
     {$DEFINE NO_EXPORTS}
   {$ENDIF MSDOS}
   {$IFDEF DPMI}
     {$DEFINE BP_DPMI}
   {$ENDIF}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver70}

{$IFDEF ver80}
   {$DEFINE Delphi}      {Delphi 1.x}
   {$DEFINE Delphi16}
   {$DEFINE Win16}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver80}

{$IFDEF ver90}
   {$DEFINE Delphi}      {Delphi 2.x}
   {$DEFINE Delphi32}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver90}

{$IFDEF ver100}
   {$DEFINE Delphi}      {Delphi 3.x}
   {$DEFINE Delphi32}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver100}

{$IFDEF ver93}
   {$DEFINE Delphi}      {C++ Builder 1.x}
   {$DEFINE Delphi32}
   {$DEFINE WINDOWS}
{$ENDIF ver93}

{$IFDEF ver110}
   {$DEFINE Delphi}      {C++ Builder 3.x}
   {$DEFINE Delphi32}
   {$DEFINE WINDOWS}
{$ENDIF ver110}

{$IFDEF ver120}
   {$DEFINE Delphi}      {Delphi 4.x}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver120}

{$IFDEF ver130}
   {$DEFINE Delphi}      {Delphi 5.x}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver130}

{$IFDEF ver140}
   {$DEFINE Delphi}      {Delphi 6.x}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver140}

{$IFDEF ver150}
   {$DEFINE Delphi}      {Delphi 7.x}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types in Delphi 7}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver150}

{$IFDEF ver160}
   {$DEFINE Delphi}      {Delphi 8}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver160}

{$IFDEF ver170}
   {$DEFINE Delphi}      {Delphi 2005}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types in Delphi 7}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver170}

{$IFDEF ver180}
   {$DEFINE Delphi}      {Delphi 2006}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types in Delphi 7}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver180}

{$IFDEF ver185}
   {$DEFINE Delphi}      {Delphi 2007}
   {$DEFINE Delphi32}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types in Delphi 7}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver180}

{$IFDEF UNIX}
  {$ifdef VER140}    // Kylix 1 & 2
    {$DEFINE KYLIX}
    {$DEFINE KYLIX1UP}
    {$DEFINE KYLIX2UP}
    {$DEFINE HAS_TYPES}
  {$endif}

  {$ifdef VER150}   // Kylix 3
    {$DEFINE KYLIX}
    {$DEFINE KYLIX1UP}
    {$DEFINE KYLIX2UP}
    {$DEFINE KYLIX3UP}
    {$DEFINE HAS_TYPES}
  {$endif}
{$ENDIF UNIX}

{$IFDEF VirtualPascal} {  Virtual Pascal 2.x }
   {$DEFINE Delphi}    {  Use Delphi Syntax }
   {$DEFINE VP2}
   {&Delphi+}
{$ENDIF VirtualPascal}

{$IFDEF Delphi}
  {$DEFINE Windows}
  {$DEFINE USE_STDCALL}
  //{$ALIGN ON}
{$ENDIF Delphi}

{$IFDEF FPC}
  {$MODE Delphi}          { use Delphi compatibility mode }
  {$H+}
  {$PACKRECORDS C}        // Added for record
  {$MACRO ON}             // Added For OpenGL
  {$DEFINE Delphi}
  {$DEFINE UseAT}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE Has_Int64}
  {$DEFINE NOCRT}
  {$IFDEF UNIX}
     {$DEFINE fpc_unix}
  {$ELSE}
     {$DEFINE __OS_DOS__}
  {$ENDIF}
  {$IFDEF WIN32}
   {$DEFINE UseWin}
  {$ENDIF}
  {$DEFINE HAS_TYPES}
{$ENDIF FPC}

{$IFDEF Win16}
  {$K+}   {smart callbacks}
{$ENDIF Win16}

 {$IFDEF OS2}
    {$UNDEF Windows}
    {$DEFINE UseWin}
    {$DEFINE OS_BigMem}
 {$ENDIF OS2}

{$IFDEF __GPC__}
  {$UNDEF UseWin}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE NOCRT}
  {$DEFINE cdecl attribute(cdecl)}
{$ENDIF}

{$IFDEF __TMT__}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE __OS_DOS__}
  {$DEFINE UseAT}
  {$IFNDEF MSDOS}
    {$DEFINE USE_STDCALL}
  {$ENDIF}

  {$IFDEF __WIN32__}
    {$DEFINE Win32}
    {$DEFINE UseWin}
    {$DEFINE NOCRT}
    {$DEFINE Win32}
    {$IFNDEF __CON__}
      {$DEFINE Windows}
    {$ENDIF}
  {$ENDIF}

  {$A+}  // Word alignment data
  {$OA+} // Objects and structures align
{$ENDIF}

{$IFDEF Win32}
  {$DEFINE OS_BigMem}
{$ELSE Win32}
  {$IFDEF ver70}
    {$DEFINE assembler}
  {$ENDIF} { use 16-bit assembler! }
{$ENDIF Win32}

{ ************************** dos/dos-like platforms **************}
{$IFDEF Windows}
   {$DEFINE __OS_DOS__}
   {$DEFINE UseWin}
   {$DEFINE MSWINDOWS}
{$ENDIF Delphi}

{$IFDEF OS2}
   {$DEFINE __OS_DOS__}
   {$DEFINE Can_Use_DLL}
{$ENDIF Delphi}

{$IFDEF UseWin}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF Win16}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF BP_DPMI}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF USE_STDCALL}
   {$IFNDEF __TMT__}
     {$DEFINE BY_NAME}
   {$ENDIF}
{$ENDIF}

{$IFNDEF ver70}
  {$UNDEF assembler}
{$ENDIF}

{*************** define LITTLE ENDIAN platforms ********************}


{$IFDEF Delphi}
{$DEFINE IA32}
{$ENDIF}

{$IFDEF KYLIX}
{$DEFINE IA32}
{$ENDIF}

{$IFDEF FPC}
{$IFDEF FPC_LITTLE_ENDIAN}
{$DEFINE IA32}
{$ENDIF}
{$ENDIF}
