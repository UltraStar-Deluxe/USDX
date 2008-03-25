unit UConfig;

// -------------------------------------------------------------------
// Note on version comparison (for developers only):
// -------------------------------------------------------------------
// Delphi (in contrast to FPC) DOESN'T support MACROS. So we
// can't define a macro like VERSION_MAJOR(version) to extract
// parts of the version-number or to create version numbers for
// comparison purposes as with a MAKE_VERSION(maj, min, rev) macro.
// So we have to define constants for every part of the version here.
//
// In addition FPC (in contrast to delphi) DOES NOT support floating-
// point numbers in $IF compiler-directives (e.g. {$IF VERSION > 1.23})
// It also DOESN'T support arithmetic operations so we aren't able to
// compare versions this way (brackets aren't supported too):
//   {$IF VERSION > ((VER_MAJ*2)+(VER_MIN*23)+(VER_REL*1))}
//
// Hence we have to use fixed numbers in the directives. At least
// Pascal allows leading 0s so 0005 equals 5 (octals are
// preceded by & and not by 0 in FPC).
// We also fix the count of digits for each part of the version number
// to 3 (aaaiiirrr with aaa=major, iii=minor, rrr=release version)
//
// A check for a library with at least a version of 2.5.11 would look
// like this:
//   {$IF LIB_VERSION >= 002005011}
//
// If you just need to check the major version do this:
//   {$IF LIB_VERSION_MAJOR >= 23}
//
// IMPORTANT:
//   Because this unit must be included in a uses-section it is
//   not possible to use the version-numbers in this uses-clause.
//   Example:
//     interface
//     uses 
//       versions, // include this file
//       {$IF USE_UNIT_XYZ}xyz;{$IFEND}      // Error: USE_UNIT_XYZ not defined
//     const
//       {$IF USE_UNIT_XYZ}test = 2;{$IFEND} // OK
//     uses
//       {$IF USE_UNIT_XYZ}xyz;{$IFEND}      // OK
//
//   Even if this file was an include-file no constants could be declared
//   before the interface's uses clause.
//   In FPC macros {$DEFINE VER:= 3} could be used to declare the version-numbers 
//   but this is incompatible to Delphi. In addition macros do not allow expand
//   arithmetic expressions. Although you can define 
//     {$DEFINE FPC_VER:= FPC_VERSION*1000000+FPC_RELEASE*1000+FPC_PATCH}
//   the following check would fail:
//     {$IF FPC_VERSION_INT >= 002002000}
//   would fail because FPC_VERSION_INT is interpreted as a string. 
//
// PLEASE consider this if you use version numbers in $IF compiler-
// directives. Otherwise you might break portability.
// -------------------------------------------------------------------

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}
   
uses
  Sysutils;

const
  // IMPORTANT:
  // If IncludeConstants is defined, the const-sections
  // of the config-file will be included too.
  // This switch is necessary because it is not possible to
  // include the const-sections in the switches.inc.
  // switches.inc is always included before the first uses-
  // section but at that place no const-section is allowed.
  // So we have to include the config-file in switches.inc
  // with IncludeConstants undefined and in UConfig.pas with
  // IncludeConstants defined (see the note above).
  {$DEFINE IncludeConstants}

  // include config-file (defines + constants)
  {$IF Defined(MSWindows)}
    {$I ../config-win.inc}
  {$ELSEIF Defined(Linux)}
    {$I ../config-linux.inc}
  {$ELSEIF Defined(Darwin)}
    {$I ../config-macosx.inc}
  {$ELSE}
    {$MESSAGE Fatal 'Unknown OS'}
  {$IFEND}

{* Libraries *}

  VERSION_MAJOR   = 1000000;
  VERSION_MINOR   = 1000;
  VERSION_RELEASE = 1;

  (*
   * Current version of UltraStar Deluxe
   *)
  USDX_VERSION_MAJOR   = 1;
  USDX_VERSION_MINOR   = 1;
  USDX_VERSION_RELEASE = 0;
  USDX_VERSION_STATE   = 'Alpha';
  USDX_STRING = 'UltraStar Deluxe';

  (*
   * FPC_VERSION is already defined as a macro by FPC itself.
   * You should use the built-in macros
   *   FPC_VERSION (=PPC_MAJOR)
   *   FPC_RELEASE (=PPC_MINOR)
   *   FPC_PATCH   (=PPC_RELEASE)
   * instead of the PPC_* ones defined here.
   * This way Windows users do not need to set this.
   *
   * Note: It might be necessary to enable macros ({$MACRO ON} or -Sm) 
   *   first if you want to use the FPC_* macros. 
   *   In FPC 2.2.0 they work even without macros being enabled but 
   *   this might be different in other versions.
   *
   * Example (Check for version >= 2.0.1):
   *   {$IF (FPC_VERSION > 2) or ((FPC_VERSION = 2) and 
   *      ( (FPC_RELEASE > 0) or ((FPC_RELEASE = 0) and 
   *        (FPC_PATCH  >= 1)) ))}
   *     {$DEFINE FPC_VER_201_PLUS}
   *   {$ENDIF}   
   *
   * IMPORTANT: do NOT check this way:
   *   {$IF (FPC_VERSION >= 2) and (FPC_RELEASE >= 0) and (FPC_PATCH >= 1)}
   *     ...
   *   In this case version 3.0.0 does not match because Patch 0 is less than 1.
   *)

  //PPC_VERSION_MAJOR   = @PPC_VERSION_MAJOR@;
  //PPC_VERSION_MINOR   = @PPC_VERSION_MINOR@;
  //PPC_VERSION_RELEASE = @PPC_VERSION_RELEASE@;
  //PPC_VERSION = (PPC_VERSION_MAJOR * VERSION_MAJOR) +
  //              (PPC_VERSION_MINOR * VERSION_MINOR) +
  //              (PPC_VERSION_RELEASE * VERSION_RELEASE);

  {$IFDEF Delphi}
  // Delphi evaluates every $IF-directive even if it is disabled by a surrounding
  // $IF or $IFDEF so the follwing will give you an error in delphi:
  //   {$IFDEF FPC}{$IF (FPC_VERSION > 2)}...{$IFEND}{$ENDIF}
  // The reason for this error is that FPC_VERSION is not a valid constant.
  // To avoid this error, we define dummys here.
  FPC_VERSION = 0;
  FPC_RELEASE = 0;
  FPC_PATCH   = 0;
  {$ENDIF}

  {$IFDEF LAZARUS}
  LAZARUS_VERSION = (LAZARUS_VERSION_MAJOR * VERSION_MAJOR) +
                    (LAZARUS_VERSION_MINOR * VERSION_MINOR) +
                    (LAZARUS_VERSION_RELEASE * VERSION_RELEASE);
  {$ENDIF}

  {$IFDEF HaveFFMpeg}

  LIBAVCODEC_VERSION = (LIBAVCODEC_VERSION_MAJOR * VERSION_MAJOR) +
                       (LIBAVCODEC_VERSION_MINOR * VERSION_MINOR) +
                       (LIBAVCODEC_VERSION_RELEASE * VERSION_RELEASE);

  LIBAVFORMAT_VERSION = (LIBAVFORMAT_VERSION_MAJOR * VERSION_MAJOR) +
                        (LIBAVFORMAT_VERSION_MINOR * VERSION_MINOR) +
                        (LIBAVFORMAT_VERSION_RELEASE * VERSION_RELEASE);

  LIBAVUTIL_VERSION = (LIBAVUTIL_VERSION_MAJOR * VERSION_MAJOR) +
                      (LIBAVUTIL_VERSION_MINOR * VERSION_MINOR) +
                      (LIBAVUTIL_VERSION_RELEASE * VERSION_RELEASE);

  {$IFDEF HaveSWScale}
  LIBSWSCALE_VERSION = (LIBSWSCALE_VERSION_MAJOR * VERSION_MAJOR) +
                       (LIBSWSCALE_VERSION_MINOR * VERSION_MINOR) +
                       (LIBSWSCALE_VERSION_RELEASE * VERSION_RELEASE);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF HaveProjectM}  
  PROJECTM_VERSION = (PROJECTM_VERSION_MAJOR * VERSION_MAJOR) +
                     (PROJECTM_VERSION_MINOR * VERSION_MINOR) +
                     (PROJECTM_VERSION_RELEASE * VERSION_RELEASE);
  {$ENDIF}

  {$IFDEF HavePortaudio}  
  PORTAUDIO_VERSION = (PORTAUDIO_VERSION_MAJOR * VERSION_MAJOR) +
                      (PORTAUDIO_VERSION_MINOR * VERSION_MINOR) +
                      (PORTAUDIO_VERSION_RELEASE * VERSION_RELEASE);
  {$ENDIF}

function USDXVersionStr(): string;
function USDXShortVersionStr(): string;

implementation

uses
  StrUtils, Math;

function USDXShortVersionStr(): string;
begin
  Result :=
    USDX_STRING +
    IfThen(USDX_VERSION_STATE <> '', ' '+USDX_VERSION_STATE);
end;

function USDXVersionStr(): string;
begin
  Result :=
    USDX_STRING + ' V ' +
    IntToStr(USDX_VERSION_MAJOR) + '.' +
    IntToStr(USDX_VERSION_MINOR) + '.' +
    IntToStr(USDX_VERSION_RELEASE) +
    IfThen(USDX_VERSION_STATE <> '', ' '+USDX_VERSION_STATE) +
    ' Build';
end;

end.
