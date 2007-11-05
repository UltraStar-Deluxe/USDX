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
	
	if ResType = 'TEX' then begin
		if sResNameLower = 'font' then
			sFileName := GetResourcesPath + 'Fonts/Normal/eurostar_regular.png'
		else if sResNameLower = 'fontb' then
			sFileName := GetResourcesPath + 'Fonts/Bold/eurostar_regular_bold.png'
		else if sResNameLower = 'fonto' then
			sFileName := GetResourcesPath + 'Fonts/Outline 1/Outline 1.png'
		else if sResNameLower = 'fonto2' then
			sFileName := GetResourcesPath + 'Fonts/Outline 2/Outline 2.png'
		else if sResNameLower = 'crdts_bg' then
			sFileName := GetResourcesPath + 'Graphics/credits_v5_bg.png'
		else if sResNameLower = 'crdts_ovl' then
			sFileName := GetResourcesPath + 'Graphics/credits_v5_overlay.png'
		else if sResNameLower = 'crdts_blindguard' then
			sFileName := GetResourcesPath + 'Graphics/names_blindguard.png'
		else if sResNameLower = 'crdts_blindy' then
			sFileName := GetResourcesPath + 'Graphics/names_blindy.png'
		else if sResNameLower = 'crdts_canni' then
			sFileName := GetResourcesPath + 'Graphics/names_canni.png'
		else if sResNameLower = 'crdts_commandio' then
			sFileName := GetResourcesPath + 'Graphics/names_commandio.png'
		else if sResNameLower = 'crdts_lazyjoker' then
			sFileName := GetResourcesPath + 'Graphics/names_lazyjoker.png'
		else if sResNameLower = 'crdts_mog' then
			sFileName := GetResourcesPath + 'Graphics/names_mog.png'
		else if sResNameLower = 'crdts_mota' then
			sFileName := GetResourcesPath + 'Graphics/names_mota.png'
		else if sResNameLower = 'crdts_skillmaster' then
			sFileName := GetResourcesPath + 'Graphics/names_skillmaster.png'
		else if sResNameLower = 'crdts_whiteshark' then
			sFileName := GetResourcesPath + 'Graphics/names_whiteshark.png'
		else if sResNameLower = 'intro_l01' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-01.png'
		else if sResNameLower = 'intro_l02' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-02.png'
		else if sResNameLower = 'intro_l03' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-03.png'
		else if sResNameLower = 'intro_l04' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-04.png'
		else if sResNameLower = 'intro_l05' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-05.png'
		else if sResNameLower = 'intro_l06' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-06.png'
		else if sResNameLower = 'intro_l07' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-07.png'
		else if sResNameLower = 'intro_l08' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-08.png'
		else if sResNameLower = 'intro_l09' then
			sFileName := GetResourcesPath + 'Graphics/intro-l-09.png'
		else if sResNameLower = 'outro_bg' then
			sFileName := GetResourcesPath + 'Graphics/outro-bg.png'
		else if sResNameLower = 'outro_esc' then
			sFileName := GetResourcesPath + 'Graphics/outro-esc.png'
		else if sResNameLower = 'outro_exd' then
			sFileName := GetResourcesPath + 'Graphics/outro-exit-dark.png';
	end
	else if ResType = 'FNT' then begin
		if sResNameLower = 'font' then
			sFileName := GetResourcesPath + 'Fonts/Normal/eurostar_regular.dat'
		else if sResNameLower = 'fontb' then
			sFileName := GetResourcesPath + 'Fonts/Bold/eurostar_regular_bold.dat'
		else if sResNameLower = 'fonto' then
			sFileName := GetResourcesPath + 'Fonts/Outline 1/Outline 1.dat'
		else if sResNameLower = 'fonto2' then
			sFileName := GetResourcesPath + 'Fonts/Outline 2/Outline 2.dat';
	end;
	
  if FileExists(sFileName) then 
    inherited Create( sFileName, fmOpenReadWrite)
  else
		raise Exception.Create('MacResources.TResourceStream.Create: File "' + sFileName + '" not found.');
end;

end.
