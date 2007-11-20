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
  
  TDirectoryEntry = Record
    Name : WideString;
	IsDirectory : Boolean;
	IsFile : Boolean;
  end;
 
  TDirectoryEntryArray = Array of TDirectoryEntry;
	
  TPlatform = class

	  // DirectoryFindFiles returns all files matching the filter. Do not use '*' in the filter.
	  // If you set ReturnAllSubDirs = true all directories will be returned, if yout set it to false
	  // directories are completely ignored.
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; virtual; abstract;

    function TerminateIfAlreadyRunning(var WndTitle : String) : Boolean; virtual;

    function GetGamePath : WideString; virtual;
  end;

var
  Platform : TPlatform;

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

{ TPlatform }

function TPlatform.GetGamePath: WideString;
begin
  // Windows and Linux use this:
  Result := ExtractFilePath(ParamStr(0));
end;

function TPlatform.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
begin
  // Linux and Mac don't check for running apps at the moment
  Result := false;
end;

initialization

  {$IFDEF MSWINDOWS}
    Platform := TPlatformWindows.Create;
  {$ENDIF}
  {$IFDEF LINUX}
    Platform := TPlatformLinux.Create;
  {$ENDIF}
  {$IFDEF DARWIN}
    Platform := TPlatformMacOSX.Create;
  {$ENDIF}

finalization

    Platform.Free;
end.
