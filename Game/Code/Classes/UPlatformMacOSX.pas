unit UPlatformMacOSX;

// Note on directories (by eddie):
// We use subfolders of the application directory on tha mac, because:
// 1. Installation on the mac works as follows: Extract and copy an application
//    and if you don't like or need the application anymore you move the folder
//    to the trash - and you're done.
// 2. If we would use subfolders of the home directory we would have to spread our
//    files to many directories - these directories are defined by Apple, but the 
//    average user doesn't know them, beacuse he or she doesn't need to know them.
//    But for UltraStar the user must at least know the songs directory...
// 
//    Creating a subfolder directly under the home directory is not acceptable.
// 

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformMacOSX = class( TInterfacedObject, IPlatform)
  public
    Function  DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
    function  TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
    procedure Halt();
    function  GetLogPath        : WideString; 
    function  GetGameSharedPath : WideString; 
    function  GetGameUserPath   : WideString; 
    function  FindSongFile(Dir, Mask: widestring): widestring;
  end;

implementation

uses SysUtils, baseunix;

// Mac applications are packaged in directories.
// We have to cut the last two directories
// to get the application directory.
Function GetBundlePath : WideString;
var
	x,
	i : integer;
begin
  Result := ExtractFilePath(ParamStr(0));
  for x := 0 to 2 do begin
    i := Length(Result);
    repeat
      Delete( Result, i, 1);
      i := Length(Result);
    until (i = 0) or (Result[i] = '/');
  end;
end;

function TPlatformMacOSX.GetLogPath        : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetBundlePath + '/Logs';
end;

function TPlatformMacOSX.GetGameSharedPath : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetBundlePath;
end;

function TPlatformMacOSX.GetGameUserPath   : WideString;
begin
  // eddie: Please read the note at the top of this file, why we use the application directory and not the user directory.
  Result := GetBundlePath;
end;

Function TPlatformMacOSX.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
    i : Integer;
    TheDir  : pdir;
    ADirent : pDirent;
    lAttrib   : integer;		
begin
  i := 0;
  Filter := LowerCase(Filter);

	TheDir := FPOpenDir(Dir);     
	if Assigned(TheDir) then
  repeat
		ADirent := FPReadDir(TheDir);

	  If Assigned(ADirent) and (ADirent^.d_name <> '.') and (ADirent^.d_name <> '..') then
		begin
			lAttrib := FileGetAttr(Dir + ADirent^.d_name);
			if ReturnAllSubDirs and ((lAttrib and faDirectory) <> 0) then
			begin
			  SetLength( Result, i + 1);
				Result[i].Name        := ADirent^.d_name;
				Result[i].IsDirectory := true;
				Result[i].IsFile      := false;
				i := i + 1;
			end
			else if (Length(Filter) = 0) or (Pos( Filter, LowerCase(ADirent^.d_name)) > 0) then
	    begin
			  SetLength( Result, i + 1);
				Result[i].Name        := ADirent^.d_name;
				Result[i].IsDirectory := false;
				Result[i].IsFile      := true;
				i := i + 1;
			end;
		end;
	Until ADirent = nil;

	FPCloseDir(TheDir);
end;

function TPlatformMacOSX.TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
begin
  result := false;
end;


procedure TPlatformMacOSX.Halt;
begin
  System.Halt;
end;

function TPlatformMacOSX.FindSongFile(Dir, Mask: widestring): widestring;
var
  SR:     TSearchRec;   // for parsing song directory
begin
  Result := '';
  if SysUtils.FindFirst(Dir + Mask, faDirectory, SR) = 0 then begin
    Result := SR.Name;
  end; // if
  SysUtils.FindClose(SR);
end;


end.
