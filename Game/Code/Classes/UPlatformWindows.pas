unit UPlatformWindows;

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

uses SysUtils, Windows;

Function TPlatform.DirectoryFindFiles(Dir, Filter : WideString; ReturnAllSubDirs : Boolean) : TDirectoryEntryArray; 
var
    i : Integer;
    SR : TSearchRecW;
begin
  i := 0;
  Filter := LowerCase(Filter);

  if ReturnAllSubDirs then begin
    if FindFirstW(Dir + '*', faDirectory, SR) = 0 then
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        SetLength( Result, i + 1);
        Result[i].Name        := SR.Name;
        Result[i].IsDirectory := true;
        Result[i].IsFile      := false;
        i := i + 1;
      end;
    until FindNextW(SR) <> 0;
    FindCloseW(SR);
  end;
  
  if FindFirstW(Dir + '*' + Filter, 0, SR) = 0 then
  repeat
    SetLength( Result, i + 1);
    Result[i].Name        := SR.Name;
    Result[i].IsDirectory := true;
    Result[i].IsFile      := false;
    i := i + 1;
  until FindNextW(SR) <> 0;
  FindCloseW(SR);
end;

end.
