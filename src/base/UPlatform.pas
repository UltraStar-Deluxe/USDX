{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UPlatform;

// Comment by Eddie:
// This unit defines an interface for platform specific utility functions.
// The Interface is implemented in separate files for each platform:
// UPlatformWindows, UPlatformLinux and UPlatformMacOSX.

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UPath;

type
  TPlatform = class
    function GetExecutionDir(): IPath;
    procedure Init; virtual;

    function TerminateIfAlreadyRunning(var WndTitle: string): boolean; virtual;
    procedure Halt; virtual;

    function GetLogPath:        IPath; virtual; abstract;
    function GetGameSharedPath: IPath; virtual; abstract;
    function GetGameUserPath:   IPath; virtual; abstract;
  end;

  function Platform(): TPlatform;

implementation

uses
  SysUtils,
  {$IF Defined(MSWINDOWS)}
  UPlatformWindows,
  {$ELSEIF Defined(DARWIN)}
  UPlatformMacOSX,
  {$ELSEIF Defined(UNIX)}
  UPlatformLinux,
  {$IFEND}
  ULog,
  UUnicodeUtils,
  UFilesystem;


// I modified it to use the Platform_singleton in this location (in the implementation)
// so that this variable can NOT be overwritten from anywhere else in the application.
// the accessor function platform, emulates all previous calls to work the same way.  
var
  Platform_singleton: TPlatform;

function Platform: TPlatform;
begin
  Result := Platform_singleton;
end;

(**
 * Default Init() implementation
 *)
procedure TPlatform.Init;
begin
end;

(**
 * Default Halt() implementation
 *)
procedure TPlatform.Halt;
begin
  // Note: Application.terminate is NOT the same
  System.Halt;
end;

{**
 * Returns the directory of the executable
 *}
function TPlatform.GetExecutionDir(): IPath;
var
  ExecName, ExecDir: IPath;
begin
  ExecName := Path(ParamStr(0));
  ExecDir := ExecName.GetPath;
  Result := ExecDir.GetAbsolutePath();
end;

(**
 * Default TerminateIfAlreadyRunning() implementation
 *)
function TPlatform.TerminateIfAlreadyRunning(var WndTitle: string): boolean;
begin
  Result := false;
end;

initialization
{$IF Defined(MSWINDOWS)}
  Platform_singleton := TPlatformWindows.Create;
{$ELSEIF Defined(DARWIN)}
  Platform_singleton := TPlatformMacOSX.Create;
{$ELSEIF Defined(UNIX)}
  Platform_singleton := TPlatformLinux.Create;
{$IFEND}

finalization
  Platform_singleton.Free;

end.
