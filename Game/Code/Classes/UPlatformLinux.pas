unit UPlatformLinux;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPlatform;

type

  TPlatform = class(TInterfacedObject, IPlatform)
  public
    Function DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
  end;

implementation

uses SysUtils, oldlinux;

Function TPlatform.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
var
    i : Integer;
    TheDir  : oldlinux.pdir;
    ADirent : oldlinux.pDirent;
    Entry   : Longint;
    info    : oldlinux.stat;
    lAttrib   : integer;		
begin
  i := 0;
  Filter := LowerCase(Filter);

  TheDir := oldlinux.opendir( Dir );     
  if Assigned(TheDir) then
  repeat
    ADirent :=  oldlinux.ReadDir(TheDir);

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

  oldlinux.CloseDir(TheDir);
end;

end.
