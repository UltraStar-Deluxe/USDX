unit MacClasses;

{$I switches.inc}

interface

uses
    Classes, GlueWindows, SysUtils;

type

  TMemoryStream = Classes.TMemoryStream;

  TResourceStream = class(TFileStream)
  private
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
  end;

{$IFDEF MACOS}
  TWndMethod = procedure of object;
{$ENDIF}

  function AllocateHWnd(Method: TWndMethod): HWND;

implementation

uses UPliki;

{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);
var
    sFileName : String;
begin
    if ResType = 'FNT' then
        sFileName := GetResourcesPath + 'Fonts/' + ResName + '.dat'
    else
        sFileName := GetResourcesPath + 'Fonts/' + ResName + '.' + ResType;

    if FileExists(sFileName) then
        inherited Create( sFileName, fmOpenReadWrite)
    else
        inherited Create( sFileName, fmCreate);
end;

function AllocateHWnd(Method: TWndMethod): HWND;
begin
{$IFDEF MSWINDOWS}
    Result := Classes.AllocateHWnd(Method);
{$ENDIF}
{$IFDEF MACOS}
    Result := 0;
{$ENDIF}
end;

end.
