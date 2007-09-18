unit UDLLManager;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


interface

uses ModiSDK,
     UFiles;

type
  TDLLMan = class
    private
      hLib: THandle;
      P_Init:   fModi_Init;
      P_Draw:   fModi_Draw;
      P_Finish: fModi_Finish;
      P_RData:  pModi_RData;
    public
      Plugins: array of TPluginInfo;
      PluginPaths: array of String;
      Selected: ^TPluginInfo;

      constructor Create;

      procedure GetPluginList;
      procedure ClearPluginInfo(No: Cardinal);
      function  LoadPluginInfo(Filename: String; No: Cardinal): boolean;

      function  LoadPlugin(No: Cardinal): boolean;
      procedure UnLoadPlugin;

      function  PluginInit   (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const LoadTex: fModi_LoadTex; const Print: fModi_Print; LoadSound: fModi_LoadSound; PlaySound: pModi_PlaySound): boolean;
      function  PluginDraw   (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean;
      function  PluginFinish (var Playerinfo: TPlayerinfo): byte;
      procedure PluginRData  (handle: HSTREAM; buffer: Pointer; len: DWORD; user: DWORD);
  end;

var
  DLLMan: TDLLMan;

const DLLPath = 'Plugins\';

implementation
uses Windows, ULog, SysUtils;


constructor TDLLMan.Create;
begin
  SetLength(Plugins, 0);
  SetLength(PluginPaths, Length(Plugins));
  GetPluginList;
end;

procedure TDLLMan.GetPluginList;
var
  SR:     TSearchRec;
begin

  if FindFirst(DLLPath + '*.dll', faAnyFile	, SR) = 0 then
  begin
    repeat
      SetLength(Plugins, Length(Plugins)+1);
      SetLength(PluginPaths, Length(Plugins));

      if LoadPluginInfo(SR.Name, High(Plugins)) then //Loaded succesful
      begin
        PluginPaths[High(PluginPaths)] := SR.Name;
      end
      else //Error Loading
      begin
        SetLength(Plugins, Length(Plugins)-1);
        SetLength(PluginPaths, Length(Plugins));
      end;

    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure TDLLMan.ClearPluginInfo(No: Cardinal);
begin
  //Set to Party Modi Plugin
  Plugins[No].Typ := 8;

  Plugins[No].Name := 'unknown';
  Plugins[No].NumPlayers := 0;

  Plugins[No].Creator := 'Nobody';
  Plugins[No].PluginDesc := 'NO_PLUGIN_DESC';

  Plugins[No].LoadSong := True;
  Plugins[No].ShowScore := True;
  Plugins[No].ShowBars := False;
  Plugins[No].ShowNotes := True;
  Plugins[No].LoadVideo := True;
  Plugins[No].LoadBack  := True;

  Plugins[No].TeamModeOnly := False;
  Plugins[No].GetSoundData := False;
  Plugins[No].Dummy := False;


  Plugins[No].BGShowFull := False;
  Plugins[No].BGShowFull_O := True;

  Plugins[No].ShowRateBar:= False;
  Plugins[No].ShowRateBar_O := True;

  Plugins[No].EnLineBonus := False;
  Plugins[No].EnLineBonus_O := True;
end;

function TDLLMan.LoadPluginInfo(Filename: String; No: Cardinal): boolean;
var
  hLibg: THandle;
  Info: pModi_PluginInfo;
  I: Integer;
begin
  Result := False;
  //Clear Plugin Info
  ClearPluginInfo(No);

  {//Workaround Plugins Loaded 2 Times
  For I := low(PluginPaths) to high(PluginPaths) do
    if (PluginPaths[I] = Filename) then
      exit;  }

  //Load Libary
  hLibg := LoadLibrary(PChar(DLLPath + Filename));
  //If Loaded
  if (hLibg <> 0) then
  begin
    //Load Info Procedure
    @Info := GetProcAddress (hLibg, PChar('PluginInfo'));

    //If Loaded
    if (@Info <> nil) then
    begin
      //Load PluginInfo
      Info (Plugins[No]);
      Result := True;
    end
    else
      Log.LogError('Could not Load Plugin "' + Filename + '": Info Procedure not Found');

    FreeLibrary (hLibg);
  end
    else
      Log.LogError('Could not Load Plugin "' + Filename + '": Libary not Loaded');
end;

function TDLLMan.LoadPlugin(No: Cardinal): boolean;
begin
  Result := False;
  //Load Libary
  hLib := LoadLibrary(PChar(DLLPath + PluginPaths[No]));
  //If Loaded
  if (hLib <> 0) then
  begin
    //Load Info Procedure
    @P_Init := GetProcAddress (hLib, PChar('Init'));
    @P_Draw := GetProcAddress (hLib, PChar('Draw'));
    @P_Finish := GetProcAddress (hLib, PChar('Finish'));

    //If Loaded
    if (@P_Init <> nil) And (@P_Draw <> nil) And (@P_Finish <> nil) then
    begin
      Selected := @Plugins[No]; 
      Result := True;
    end
    else
    begin
      Log.LogError('Could not Load Plugin "' + PluginPaths[No] + '": Procedures not Found');

    end;
  end
    else
      Log.LogError('Could not Load Plugin "' + PluginPaths[No] + '": Libary not Loaded');
end;

procedure TDLLMan.UnLoadPlugin;
begin
if (hLib <> 0) then
  FreeLibrary (hLib);

//Selected := nil;
@P_Init := nil;
@P_Draw := nil;
@P_Finish := nil;
@P_RData := nil;
end;

function TDLLMan.PluginInit (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const LoadTex: fModi_LoadTex; const Print: fModi_Print; LoadSound: fModi_LoadSound; PlaySound: pModi_PlaySound): boolean;
var
  Methods: TMethodRec;
begin
  Methods.LoadTex := LoadTex;
  Methods.Print := Print;
  Methods.LoadSound := LoadSound;
  Methods.PlaySound := PlaySound;
  
  if (@P_Init <> nil) then
    Result := P_Init (TeamInfo, PlayerInfo, Sentences, Methods)
  else
    Result := False
end;

function TDLLMan.PluginDraw   (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean;
begin
if (@P_Draw <> nil) then
  Result := P_Draw (PlayerInfo, CurSentence)
else
  Result := False
end;

function TDLLMan.PluginFinish (var Playerinfo: TPlayerinfo): byte;
begin
if (@P_Finish <> nil) then
  Result := P_Finish (PlayerInfo)
else
  Result := 0;
end;

procedure TDLLMan.PluginRData  (handle: HSTREAM; buffer: Pointer; len: DWORD; user: DWORD);
begin
if (@P_RData <> nil) then
  P_RData (handle, buffer, len, user);
end;

end.
