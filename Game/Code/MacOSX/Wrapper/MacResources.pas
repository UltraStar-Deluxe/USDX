unit MacResources;

{$I switches.inc}

interface

uses
    Classes, Windows, SysUtils;

type

  TResourceStream = class(TFileStream)
  private
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
  end;
  
  Function FindResource( hInstance : THandle; pcIdentifier : PChar; pcResType : PChar) : THandle;

implementation

Function FindResource( hInstance : THandle; pcIdentifier : PChar; pcResType : PChar) : THandle;
begin
	Result := 1;
end;

Function GetResourcesPath : String;
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

{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);
var
    sResNameLower : string;
    sFileName : String;
begin
	sResNameLower := LowerCase(string(ResName));

    if sResNameLower = 'font' then
        sFileName := GetResourcesPath + 'Fonts/Normal/Font Normal 16.png'
    else if sResNameLower = 'fontb' then
        sFileName := GetResourcesPath + 'Fonts/Bold/Font 1024 Bold 16.png'
    else if sResNameLower = 'fonto' then
        sFileName := GetResourcesPath + 'Fonts/Outline 1/Outline 1.png'
    else if sResNameLower = 'outro_bg' then
        sFileName := GetResourcesPath + 'Graphics/outro-bg.png'
    else if sResNameLower = 'outro_esc' then
        sFileName := GetResourcesPath + 'Graphics/outro-esc.png'
    else if sResNameLower = 'outro_exd' then
        sFileName := GetResourcesPath + 'Graphics/outro-exit-dark.png'
    else if sResNameLower = 'fonto2' then
        sFileName := GetResourcesPath + 'Fonts/Outline 2/Outline 2.png';

    if FileExists(sFileName) then 
        inherited Create( sFileName, fmOpenReadWrite)
    else
		raise Exception.Create('MacResources.TResourceStream.Create: File "' + sFileName + '" not found.');
end;

end.
