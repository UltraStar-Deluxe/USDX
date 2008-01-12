unit UPlatformMacOSX;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatformMacOSX = class( TInterfacedObject, IPlatform)
  private
  public
<<<<<<< .mine
    Function  DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; override;
    function  GetGamePath: WideString; override;
    function  TerminateIfAlreadyRunning(var WndTitle : String) : Boolean;
    procedure halt();
=======
    function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; override;
    function GetLogPath        : WideString; override;
    function GetGameSharedPath : WideString; override;
    function GetGameUserPath   : WideString; override;
>>>>>>> .r788
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
  Result := GetBundlePath;
end;

function TPlatformMacOSX.GetGameSharedPath : WideString;
begin
  Result := GetBundlePath;
end;

function TPlatformMacOSX.GetGameUserPath   : WideString;
begin
  Result := GetBundlePath;
end;

Function TPlatformMacOSX.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray;
var
    i : Integer;
    TheDir  : pdir;
    ADirent : pDirent;
    Entry   : Longint;
    info    : stat;
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


procedure TPlatformMacOSX.halt;
begin
  halt;
end;

end.
