unit UPlatformMacOSX;

{
 Note on directories (by eddie):
 We use subfolders of the application directory on tha mac, because:
 1. Installation on the mac works as follows: Extract and copy an application
    and if you don't like or need the application anymore you move the folder
    to the trash - and you're done.
 2. If we would use subfolders of the home directory we would have to spread our
    files to many directories - these directories are defined by Apple, but the 
    average user doesn't know them, beacuse he or she doesn't need to know them.
    But for UltraStar the user must at least know the songs directory...
 
    Creating a subfolder directly under the home directory is not acceptable.

 More on this from KMS aka mischi

 Handling of resources and folders should follow these lines.

 Acceptable places for files are folders named UltraStarDeluxe either in
 /Library/Application Support/
 or 
 ~/Library/Application Support/
 
 Then 
 GetGameSharedPath could return 
 /Library/Application Support/UltraStarDeluxe/Resources/
 GetGameUserPath could return 
 ~/Library/Application Support/UltraStarDeluxe/Resources/

 USDX checks, whether GetGameUserPath exists. If not, USDX creates its.
 The existance of needed files is then checked and if a file is missing
 it is copied to there from within the Resources folder in the Application 
 bundle, which contains the default files. USDX should not delete files or 
 folders in Application Support/UltraStarDeluxe automatically or without 
 user confirmation.
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformMacOSX = class(TInterfacedObject, IPlatform)
  public
    function  DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : boolean) : TDirectoryEntryArray; 
    function  TerminateIfAlreadyRunning(var WndTitle : string) : boolean;
    procedure Halt();
    function  GetLogPath        : WideString; 
    function  GetGameSharedPath : WideString; 
    function  GetGameUserPath   : WideString; 
    function  FindSongFile(Dir, Mask: WideString): WideString;
  end;

implementation

uses SysUtils, baseunix;

// Mac applications are packaged in directories.
// We have to cut the last two directories
// to get the application directory.

function GetBundlePath : WideString;
var
  i, pos : integer;
begin
  Result := ExtractFilePath(ParamStr(0));
  for i := 1 to 2 do
  begin
    pos := Length(Result);
    repeat
      Delete(Result, pos, 1);
      pos := Length(Result);
    until (pos = 0) or (Result[pos] = '/');
  end;
end;

function GetApplicationSupportPath : WideString;
const
  PathName : string = '/Library/Application Support/UltraStarDeluxe/Resources';
begin
  Result := GetEnvironmentVariable('HOME') + PathName + '/';
end;

function TPlatformMacOSX.GetLogPath        : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetApplicationSupportPath + 'Logs';
end;

function TPlatformMacOSX.GetGameSharedPath : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetApplicationSupportPath;
end;

function TPlatformMacOSX.GetGameUserPath   : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetApplicationSupportPath;
end;

function TPlatformMacOSX.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : boolean) : TDirectoryEntryArray;
var
  i       : integer;
  TheDir  : pdir;
  ADirent : pDirent;
  lAttrib : integer;		
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := FPOpenDir(Dir);     
  if Assigned(TheDir) then
    repeat
      ADirent := FPReadDir(TheDir);

      if Assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
      begin
	lAttrib := FileGetAttr(Dir + ADirent^.d_name);
	if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
	begin
	  SetLength(Result, i + 1);
	  Result[i].Name        := ADirent^.d_name;
	  Result[i].IsDirectory := true;
	  Result[i].IsFile      := false;
	  i := i + 1;
	end
	else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(ADirent^.d_name)) > 0) then
	begin
	  SetLength(Result, i + 1);
	  Result[i].Name        := ADirent^.d_name;
	  Result[i].IsDirectory := false;
	  Result[i].IsFile      := true;
	  i := i + 1;
	end;
      end;
    until ADirent = nil;

  FPCloseDir(TheDir);
end;

function TPlatformMacOSX.TerminateIfAlreadyRunning(var WndTitle : string) : boolean;
begin
  result := false;
end;

procedure TPlatformMacOSX.Halt;
begin
  System.Halt;
end;

function TPlatformMacOSX.FindSongFile(Dir, Mask: WideString): WideString;
var
  SR : TSearchRec;   // for parsing song directory
begin
  Result := '';
  // faDirectory = $00000010; Attribute of a Þle, meaning the Þle is a directory.
  if SysUtils.FindFirst(Dir + Mask, faDirectory, SR) = 0 then
  begin
    Result := SR.Name;
  end; // if
  SysUtils.FindClose(SR);
end;

end.
