unit UPlatform;

// Comment by Eddie:
// This unit defines an interface for platform specific utility functions.
// The Interface is implemented in separate files for each platform:
// UPlatformWindows, UPlatformLinux and UPlatformWindows.

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes;

type
  TDirectoryEntry = record
    Name        : WideString;
    IsDirectory : boolean;
    IsFile      : boolean;
  end;

  TDirectoryEntryArray = array of TDirectoryEntry;

  TPlatform = class
    procedure Init; virtual;
    function  DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: boolean): TDirectoryEntryArray; virtual; abstract;
    function  TerminateIfAlreadyRunning(var WndTitle : string): boolean; virtual;
    function  FindSongFile(Dir, Mask: WideString): WideString; virtual;
    procedure Halt; virtual;
    function  GetLogPath        : WideString; virtual; abstract;
    function  GetGameSharedPath : WideString; virtual; abstract;
    function  GetGameUserPath   : WideString; virtual; abstract;
  end;

  function Platform(): TPlatform;

implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  UPlatformWindows;
  {$ENDIF}
  {$IFDEF LINUX}
  UPlatformLinux;
  {$ENDIF}
  {$IFDEF DARWIN}
  UPlatformMacOSX;
  {$ENDIF}


// I have modified it to use the Platform_singleton in this location ( in the implementaiton )
// so that this variable can NOT be overwritten from anywhere else in the application.
// the accessor function platform, emulates all previous calls to work the same way.  
var
  Platform_singleton : TPlatform;

function Platform : TPlatform;
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

(**
 * Default TerminateIfAlreadyRunning() implementation
 *)
function TPlatform.TerminateIfAlreadyRunning(var WndTitle : string): Boolean;
begin
  Result := false;
end;

(**
 * Default FindSongFile() implementation
 *)
function TPlatform.FindSongFile(Dir, Mask: WideString): WideString;
var
  SR: TSearchRec;   // for parsing song directory
begin
  Result := '';
  if SysUtils.FindFirst(Dir + Mask, faDirectory, SR) = 0 then
  begin
    Result := SR.Name;
  end;
  SysUtils.FindClose(SR);
end;


initialization
{$IFDEF MSWINDOWS}
  Platform_singleton := TPlatformWindows.Create;
{$ENDIF}
{$IFDEF LINUX}
  Platform_singleton := TPlatformLinux.Create;
{$ENDIF}
{$IFDEF DARWIN}
  Platform_singleton := TPlatformMacOSX.Create;
{$ENDIF}

finalization
  Platform_singleton.Free;

end.
