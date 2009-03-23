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
  Classes;

type
  TDirectoryEntry = record
    Name:        WideString;
    IsDirectory: boolean;
    IsFile:      boolean;
  end;

  TDirectoryEntryArray = array of TDirectoryEntry;

  TPlatform = class
    function GetExecutionDir(): string;
    procedure Init; virtual;
    function DirectoryFindFiles(Dir, Filter: WideString; ReturnAllSubDirs: boolean): TDirectoryEntryArray; virtual; abstract;
    function TerminateIfAlreadyRunning(var WndTitle: string): boolean; virtual;
    function FindSongFile(Dir, Mask: WideString): WideString; virtual;
    procedure Halt; virtual;
    function GetLogPath:        WideString; virtual; abstract;
    function GetGameSharedPath: WideString; virtual; abstract;
    function GetGameUserPath:   WideString; virtual; abstract;
    function CopyFile(const Source, Target: WideString; FailIfExists: boolean): boolean; virtual;
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
  ULog;


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
function TPlatform.GetExecutionDir(): string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)));
end;

(**
 * Default TerminateIfAlreadyRunning() implementation
 *)
function TPlatform.TerminateIfAlreadyRunning(var WndTitle: string): boolean;
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

function TPlatform.CopyFile(const Source, Target: WideString; FailIfExists: boolean): boolean;
const
  COPY_BUFFER_SIZE = 4096; // a good tradeoff between speed and memory consumption
var
  SourceFile, TargetFile: TFileStream;
  FileCopyBuffer: array [0..COPY_BUFFER_SIZE-1] of byte; // temporary copy-buffer.
  NumberOfBytes:  integer; // number of bytes read from SourceFile
begin
  Result := false;
  SourceFile := nil;
  TargetFile := nil;

  // if overwrite is disabled return if the target file already exists
  if (FailIfExists and FileExists(Target)) then
    Exit;

  try
    try
      // open source and target file (might throw an exception on error)
      SourceFile := TFileStream.Create(Source, fmOpenRead);
      TargetFile := TFileStream.Create(Target, fmCreate or fmOpenWrite);

      while true do
      begin
        // read a block from the source file and check for errors or EOF
        NumberOfBytes := SourceFile.Read(FileCopyBuffer, SizeOf(FileCopyBuffer));
        if (NumberOfBytes <= 0) then
          Break;
        // write block to target file and check if everything was written
        if (TargetFile.Write(FileCopyBuffer, NumberOfBytes) <> NumberOfBytes) then
          Exit;
      end;
    except
      Exit;
    end;
  finally
    SourceFile.Free;
    TargetFile.Free;
  end;

  Result := true;
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
