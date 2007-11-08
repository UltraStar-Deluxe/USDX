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
	
  IPlatform = interface
  
	// DirectoryFindFiles returns all files matching the filter. Do not use '*' in the filter.
	// If you set ReturnAllSubDirs = true all directories will be returned, if yout set it to false
	// directories are completely ignored. 
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
  end;

var
  Platform : IPlatform;

implementation

uses
  {$IFDEF MSWINDOWS}
  UPlatformWindows;
  {$ENDIF}
  {$IFDEF LINUX}
  UPlatformLinux;
  {$ENDIF}
  {$IFDEF DARWIN}
  UPlatformMacOSX;
  {$ENDIF}

initialization

  Platform := TPlatform.Create;

end.
