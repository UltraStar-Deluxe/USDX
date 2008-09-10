unit UPluginLoader;
{*********************
  UPluginLoader
  Unit contains to Classes
    TPluginLoader: Class Searching for and Loading the Plugins
    TtehPlugins:   Class that represents the Plugins in Modules chain
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UPluginDefs,
  UCoreModule;

type
  TPluginListItem = record
    Info: TUS_PluginInfo;
    State: Byte; //State of this Plugin: 0 - undefined; 1 - Loaded; 2 - Inited / Running; 4 - Unloaded; 254 - Loading aborted by Plugin; 255 - Unloaded because of Error
    Path: String; //Path to this Plugin
    NeedsDeInit: Boolean; //If this is Inited correctly this should be true
    hLib: THandle; //Handle of Loaded Libary
    Procs: record //Procs offered by Plugin. Don't call this directly use wrappers of TPluginLoader
      Load:   Func_Load;
      Init:   Func_Init;
      DeInit: Proc_DeInit;
    end;
  end;
  {*********************
    TPluginLoader
    Class Searches for Plugins and Manages loading and unloading
  *********************}
  PPluginLoader = ^TPluginLoader;
  TPluginLoader = class (TCoreModule)
    private
      LoadingProcessFinished: Boolean;
      sUnloadPlugin:    THandle;
      sLoadPlugin:      THandle;
      sGetPluginInfo:   THandle;
      sGetPluginState:  THandle;

      procedure FreePlugin(Index: Cardinal);
    public
      PluginInterface: TUS_PluginInterface;
      Plugins: array of TPluginListItem;

      //TCoreModule methods to inherit
      constructor Create; override;
      procedure Info(const pInfo: PModuleInfo); override;
      function Load: Boolean; override;
      function Init: Boolean; override;
      procedure DeInit; override;
      Destructor Destroy; override;

      //New Methods
      procedure BrowseDir(Path: String);             //Browses the Path at _Path_ for Plugins
      function  PluginExists(Name: String): integer; //If Plugin Exists: Index of Plugin, else -1
      procedure AddPlugin(Filename: String);//Adds Plugin to the Array

      function  CallLoad(Index: Cardinal): integer;
      function  CallInit(Index: Cardinal): integer;
      procedure CallDeInit(Index: Cardinal);

      //Services offered
      function LoadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      function UnloadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      function GetPluginInfo(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Get PluginInfo of Plugin with Index(wParam) to Address at lParam)
      function GetPluginState(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Return PluginInfo of Plugin with Index(wParam))

  end;

  {*********************
    TtehPlugins
    Class Represents the Plugins in Module Chain.
    It Calls the Plugins Procs and Funcs
  *********************}
  TtehPlugins = class (TCoreModule)
    private
      PluginLoader: PPluginLoader;
    public
      //TCoreModule methods to inherit
      constructor Create; override;
      
      procedure Info(const pInfo: PModuleInfo); override;
      function Load: Boolean; override;
      function Init: Boolean; override;
      procedure DeInit; override;
  end;

const
{$IF Defined(MSWINDOWS)}
  PluginFileExtension = '.dll';
{$ELSEIF Defined(DARWIN)}
  PluginFileExtension = '.dylib';
{$ELSEIF Defined(UNIX)}
  PluginFileExtension = '.so';
{$IFEND}

implementation

uses
  UCore,
  UPluginInterface,
{$IFDEF MSWINDOWS}
  windows,
{$ELSE}
  dynlibs,
{$ENDIF}
  UMain,
  SysUtils;

{*********************
  TPluginLoader
  Implentation
*********************}

//-------------
// function that gives some Infos about the Module to the Core
//-------------
procedure TPluginLoader.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TPluginLoader';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Searches for Plugins, loads and unloads them';
end;

//-------------
// Just the Constructor
//-------------
constructor TPluginLoader.Create;
begin
  inherited;

  //Init PluginInterface
  //Using Methods from UPluginInterface
  PluginInterface.CreateHookableEvent := CreateHookableEvent;
  PluginInterface.DestroyHookableEvent := DestroyHookableEvent;
  PluginInterface.NotivyEventHooks := NotivyEventHooks;
  PluginInterface.HookEvent := HookEvent;
  PluginInterface.UnHookEvent := UnHookEvent;
  PluginInterface.EventExists := EventExists;

  PluginInterface.CreateService := @CreateService;
  PluginInterface.DestroyService := DestroyService;
  PluginInterface.CallService := CallService;
  PluginInterface.ServiceExists := ServiceExists;

  //UnSet Private Var
  LoadingProcessFinished := False;
end;

//-------------
//Is Called on Loading.
//In this Method only Events and Services should be created
//to offer them to other Modules or Plugins during the Init process
//If False is Returned this will cause a Forced Exit
//-------------
function TPluginLoader.Load: Boolean;
begin
  Result := True;

  try
    //Start Searching for Plugins
    BrowseDir(PluginPath);
  except
    Result := False;
    Core.ReportError(integer(PChar('Error Browsing and Loading.')), PChar('TPluginLoader'));
  end;
end;

//-------------
//Is Called on Init Process
//In this Method you can Hook some Events and Create + Init
//your Classes, Variables etc.
//If False is Returned this will cause a Forced Exit
//-------------
function TPluginLoader.Init: Boolean;
begin
  //Just set Prvate Var to true.
  LoadingProcessFinished := True;
  Result := True;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
procedure TPluginLoader.DeInit;
var
  I: integer;
begin
  //Force DeInit
  //If some Plugins aren't DeInited for some Reason o0
  for I := 0 to High(Plugins) do
  begin
    if (Plugins[I].State < 4) then
      FreePlugin(I);
  end;

  //Nothing to do here. Core will remove the Hooks
end;

//-------------
//Is Called if this Module will be unloaded and has been created
//Should be used to Free Memory
//-------------
Destructor TPluginLoader.Destroy;
begin
  //Just save some Memory if it wasn't done now..
  SetLength(Plugins, 0);
  inherited;
end;

//--------------
// Browses the Path at _Path_ for Plugins
//--------------
procedure TPluginLoader.BrowseDir(Path: String);
var
  SR: TSearchRec;
begin
  //Search for other Dirs to Browse
  if FindFirst(Path + '*', faDirectory, SR) = 0 then begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        BrowseDir(Path + Sr.Name + PathDelim);
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);
  
  //Search for Plugins at Path
  if FindFirst(Path + '*' + PluginFileExtension, 0, SR) = 0 then
  begin
    repeat
      AddPlugin(Path + SR.Name);
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);
end;
 
//--------------
// If Plugin Exists: Index of Plugin, else -1
//--------------
function  TPluginLoader.PluginExists(Name: String): integer;
var
  I: integer;
begin
  Result := -1;

  if (Length(Name) <= 32 { =>Length(TUS_PluginInfo.Name)}) then
  begin
    for I := 0 to High(Plugins) do
      if (Plugins[I].Info.Name = Name) then
      begin //Found the Plugin
        Result := I;
        Break;
      end;
  end;
end;
 
//--------------
// Adds Plugin to the Array
//--------------
procedure TPluginLoader.AddPlugin(Filename: String);
var
  hLib: THandle;
  PInfo: Proc_PluginInfo;
  Info: TUS_PluginInfo;
  PluginID: integer;
begin
  if (FileExists(Filename)) then
  begin //Load Libary
    hLib := LoadLibrary(PChar(Filename));
    if (hLib <> 0) then
    begin //Try to get Address of the Info Proc
      PInfo := GetProcAddress (hLib, PChar('USPlugin_Info'));
      if (@PInfo <> nil) then
      begin
        Info.cbSize := SizeOf(TUS_PluginInfo);

        try //Call Info Proc
          PInfo(@Info);
        except
          Info.Name := '';
          Core.ReportError(integer(PChar('Error getting Plugin Info: ' + Filename)), PChar('TPluginLoader'));
        end;

        //Is Name set ?
        if (Trim(Info.Name) <> '') then
        begin
          PluginID := PluginExists(Info.Name);

          if (PluginID > 0) and (Plugins[PluginID].State >=4) then
            PluginID := -1;

          if (PluginID = -1) then
          begin
            //Add new item to array
            PluginID := Length(Plugins);
            SetLength(Plugins, PluginID + 1);

            //Fill with Info:
            Plugins[PluginID].Info := Info;
            Plugins[PluginID].State := 0;
            Plugins[PluginID].Path := Filename;
            Plugins[PluginID].NeedsDeInit := False;
            Plugins[PluginID].hLib := hLib;

            //Try to get Procs
            Plugins[PluginID].Procs.Load   := GetProcAddress (hLib, PChar('USPlugin_Load'));
            Plugins[PluginID].Procs.Init   := GetProcAddress (hLib, PChar('USPlugin_Init'));
            Plugins[PluginID].Procs.DeInit := GetProcAddress (hLib, PChar('USPlugin_DeInit'));

            if (@Plugins[PluginID].Procs.Load = nil) OR (@Plugins[PluginID].Procs.Init = nil) OR (@Plugins[PluginID].Procs.DeInit = nil) then
            begin
              Plugins[PluginID].State := 255;
              FreeLibrary(hLib);
              Core.ReportError(integer(PChar('Can''t get Plugin Procs from Libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
            end;

            //Emulate loading process if this Plugin is loaded to late
            if (LoadingProcessFinished) then
            begin
              CallLoad(PluginID);
              CallInit(PluginID);
            end;
          end
          else if (LoadingProcessFinished = False) then
          begin
            if (Plugins[PluginID].Info.Version < Info.Version) then
            begin //Found newer Version of this Plugin
              Core.ReportDebug(integer(PChar('Found a newer Version of Plugin: ' + String(Info.Name))), PChar('TPluginLoader'));

              //Unload Old Plugin
              UnloadPlugin(PluginID, nil);

              //Fill with new Info
              Plugins[PluginID].Info := Info;
              Plugins[PluginID].State := 0;
              Plugins[PluginID].Path := Filename;
              Plugins[PluginID].NeedsDeInit := False;
              Plugins[PluginID].hLib := hLib;

              //Try to get Procs
              Plugins[PluginID].Procs.Load   := GetProcAddress (hLib, PChar('USPlugin_Load'));
              Plugins[PluginID].Procs.Init   := GetProcAddress (hLib, PChar('USPlugin_Init'));
              Plugins[PluginID].Procs.DeInit := GetProcAddress (hLib, PChar('USPlugin_DeInit'));

              if (@Plugins[PluginID].Procs.Load = nil) OR (@Plugins[PluginID].Procs.Init = nil) OR (@Plugins[PluginID].Procs.DeInit = nil) then
              begin
                FreeLibrary(hLib);
                Plugins[PluginID].State := 255;
                Core.ReportError(integer(PChar('Can''t get Plugin Procs from Libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
              end;
            end
            else
            begin //Newer Version already loaded
              FreeLibrary(hLib);
            end;
          end
          else
          begin
            FreeLibrary(hLib);
            Core.ReportError(integer(PChar('Plugin with this Name already exists: ' + String(Info.Name))), PChar('TPluginLoader'));
          end;
        end
        else
        begin
          FreeLibrary(hLib);
          Core.ReportError(integer(PChar('No name reported: ' + Filename)), PChar('TPluginLoader'));
        end;
      end
      else
      begin
        FreeLibrary(hLib);
        Core.ReportError(integer(PChar('Can''t find Info procedure: ' + Filename)), PChar('TPluginLoader'));
      end;
    end
    else
      Core.ReportError(integer(PChar('Can''t load Plugin Libary: ' + Filename)), PChar('TPluginLoader'));
  end;
end;

//--------------
// Calls Load Func of Plugin with the given Index
//--------------
function  TPluginLoader.CallLoad(Index: Cardinal): integer;
begin
  Result := -2;
  if(Index < Length(Plugins)) then
  begin
    if (@Plugins[Index].Procs.Load <> nil) and (Plugins[Index].State = 0) then
    begin
      try
        Result := Plugins[Index].Procs.Load(@PluginInterface);
      except
        Result := -3;
      End;

      if (Result = 0) then
        Plugins[Index].State := 1
      else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(integer(PChar('Error calling Load function from Plugin: ' + String(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls Init Func of Plugin with the given Index
//--------------
function  TPluginLoader.CallInit(Index: Cardinal): integer;
begin
  Result := -2;
  if(Index < Length(Plugins)) then
  begin
    if (@Plugins[Index].Procs.Init <> nil) and (Plugins[Index].State = 1) then
    begin
      try
        Result := Plugins[Index].Procs.Init(@PluginInterface);
      except
        Result := -3;
      End;
      
      if (Result = 0) then
      begin
        Plugins[Index].State := 2;
        Plugins[Index].NeedsDeInit := True;
      end
      else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(integer(PChar('Error calling Init function from Plugin: ' + String(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls DeInit Proc of Plugin with the given Index
//--------------
procedure TPluginLoader.CallDeInit(Index: Cardinal);
begin
  if(Index < Length(Plugins)) then
  begin
    if (Plugins[Index].State < 4) then
    begin
      if (@Plugins[Index].Procs.DeInit <> nil) and (Plugins[Index].NeedsDeInit) then
        try
          Plugins[Index].Procs.DeInit(@PluginInterface);
        except

        End;

      //Don't forget to remove Services and Subscriptions by this Plugin
      Core.Hooks.DelbyOwner(-1 - Index);

      FreePlugin(Index);
    end;
  end;
end;

//--------------
// Frees all Plugin Sources (Procs and Handles) - Helper for Deiniting Functions
//--------------
procedure TPluginLoader.FreePlugin(Index: Cardinal);
begin
  Plugins[Index].State := 4;
  Plugins[Index].Procs.Load := nil;
  Plugins[Index].Procs.Init := nil;
  Plugins[Index].Procs.DeInit := nil;

  if (Plugins[Index].hLib <> 0) then
    FreeLibrary(Plugins[Index].hLib);
end;



//--------------
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the Plugin
//--------------
function TPluginLoader.LoadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: integer;
  sFile: String;
begin
  Result := -1;
  sFile := '';
  //lParam is ID
  if (lParam = nil) then
  begin
    Index := wParam;
  end
  else
  begin //lParam is PChar
    try
      sFile := String(PChar(lParam));
      Index := PluginExists(sFile);
      if (Index < 0) And FileExists(sFile) then
      begin //Is Filename
        AddPlugin(sFile);
        Result := Plugins[High(Plugins)].State;
      end;
    except
      Index := -2;
    end;
  end;


  if (Index >= 0) and (Index < Length(Plugins)) then
  begin
    AddPlugin(Plugins[Index].Path);
    Result := Plugins[Index].State;
  end;
end;

//--------------
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the Plugin
//--------------
function TPluginLoader.UnloadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: integer;
  sName: String;
begin
  Result := -1;
  //lParam is ID
  if (lParam = nil) then
  begin
    Index := wParam;
  end
  else
  begin //wParam is PChar
    try
      sName := String(PChar(lParam));
      Index := PluginExists(sName);
    except
      Index := -2;
    end;
  end;


  if (Index >= 0) and (Index < Length(Plugins)) then
      CallDeInit(Index)
end;

//--------------
// if wParam = -1 then (if lParam = nil then get length of Moduleinfo Array. if lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Get PluginInfo of Plugin with Index(wParam) to Address at lParam)
//--------------
function TPluginLoader.GetPluginInfo(wParam: TwParam; lParam: TlParam): integer;
var I: integer;
begin
  Result := 0;
  if (wParam > 0) then
  begin //Get Info of 1 Plugin
    if (lParam <> nil) and (wParam < Length(Plugins)) then
    begin
      try
        Result := 1;
        PUS_PluginInfo(lParam)^ := Plugins[wParam].Info;
      except

      End;
    end;
  end
  else if (lParam = nil) then
  begin //Get Length of Plugin (Info) Array
    Result := Length(Plugins);
  end
  else //Write PluginInfo Array to Address in lParam
  begin
    try
      for I := 0 to high(Plugins) do
        PAUS_PluginInfo(lParam)^[I] := Plugins[I].Info;
      Result := Length(Plugins);
    except
      Core.ReportError(integer(PChar('Could not write PluginInfo Array')), PChar('TPluginLoader'));
    End;
  end;

end;

//--------------
// if wParam = -1 then (if lParam = nil then get length of Plugin State Array. if lparam <> nil then write array of Byte to address at lparam) else (Return State of Plugin with Index(wParam))
//--------------
function TPluginLoader.GetPluginState(wParam: TwParam; lParam: TlParam): integer;
var I: integer;
begin
  Result := -1;
  if (wParam > 0) then
  begin //Get State of 1 Plugin
    if (wParam < Length(Plugins)) then
    begin
      Result := Plugins[wParam].State;
    end;
  end
  else if (lParam = nil) then
  begin //Get Length of Plugin (Info) Array
    Result := Length(Plugins);
  end
  else //Write PluginInfo Array to Address in lParam
  begin
    try
      for I := 0 to high(Plugins) do
        Byte(Pointer(integer(lParam) + I)^) := Plugins[I].State;
      Result := Length(Plugins);
    except
      Core.ReportError(integer(PChar('Could not write PluginState Array')), PChar('TPluginLoader'));
    End;
  end;
end;


{*********************
  TtehPlugins
  Implentation
*********************}

//-------------
// function that gives some Infos about the Module to the Core
//-------------
procedure TtehPlugins.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TtehPlugins';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Module executing the Plugins!';
end;

//-------------
// Just the Constructor
//-------------
constructor TtehPlugins.Create;
begin
  inherited;
  PluginLoader := nil;
end;

//-------------
//Is Called on Loading.
//In this Method only Events and Services should be created
//to offer them to other Modules or Plugins during the Init process
//If False is Returned this will cause a Forced Exit
//-------------
function TtehPlugins.Load: Boolean;
var
  i: integer; //Counter
  CurExecutedBackup: integer; //backup of Core.CurExecuted Attribute
begin
  //Get Pointer to PluginLoader
  PluginLoader := PPluginLoader(Core.GetModulebyName('TPluginLoader'));
  if (PluginLoader = nil) then
  begin
    Result := false;
    Core.ReportError(integer(PChar('Could not get Pointer to PluginLoader')), PChar('TtehPlugins'));
  end
  else
  begin
    Result := true;

    //Backup CurExecuted
    CurExecutedBackup := Core.CurExecuted;

    //Start Loading the Plugins
    for i := 0 to High(PluginLoader.Plugins) do
    begin
      Core.CurExecuted := -1 - i;

      try
        //Unload Plugin if not correctly Executed
        if (PluginLoader.CallLoad(i) <> 0) then
        begin
          PluginLoader.CallDeInit(i);
          PluginLoader.Plugins[i].State := 254; //Plugin asks for unload
          Core.ReportDebug(integer(PChar('Plugin Selfabort during loading process: ' + String(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
        end
        else
        begin
          Core.ReportDebug(integer(PChar('Plugin loaded succesful: ' + String(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
        end;
      except
        //Plugin could not be loaded.
        // => Show Error Message, then ShutDown Plugin
        on E: Exception do
        begin
          PluginLoader.CallDeInit(i);
          PluginLoader.Plugins[i].State := 255; //Plugin causes Error
          Core.ReportError(integer(PChar('Plugin causes Error during loading process: ' + PluginLoader.Plugins[i].Info.Name + ', ErrorMsg: "' + E.Message + '"')), PChar('TtehPlugins'));
        end;
      end;
    end;

    //Reset CurExecuted  
    Core.CurExecuted := CurExecutedBackup;
  end;
end;

//-------------
//Is Called on Init Process
//In this Method you can Hook some Events and Create + Init
//your Classes, Variables etc.
//If False is Returned this will cause a Forced Exit
//-------------
function TtehPlugins.Init: Boolean;
var
  i: integer; //Counter
  CurExecutedBackup: integer; //backup of Core.CurExecuted Attribute
begin
  Result := true;

  //Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  //Start Loading the Plugins
  for i := 0 to High(PluginLoader.Plugins) do
    try
      Core.CurExecuted := -1 - i;

      //Unload Plugin if not correctly Executed
      if (PluginLoader.CallInit(i) <> 0) then
      begin
        PluginLoader.CallDeInit(i);
        PluginLoader.Plugins[i].State := 254; //Plugin asks for unload
        Core.ReportDebug(integer(PChar('Plugin Selfabort during init process: ' + String(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
      end
      else
        Core.ReportDebug(integer(PChar('Plugin inited succesful: ' + String(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
    except
      //Plugin could not be loaded.
      // => Show Error Message, then ShutDown Plugin
      PluginLoader.CallDeInit(i);
      PluginLoader.Plugins[i].State := 255; //Plugin causes Error
      Core.ReportError(integer(PChar('Plugin causes Error during init process: ' + String(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
    end;

  //Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
procedure TtehPlugins.DeInit;
var
  i: integer; //Counter
  CurExecutedBackup: integer; //backup of Core.CurExecuted Attribute
begin
  //Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  //Start Loop
  
  for i := 0 to High(PluginLoader.Plugins) do
  begin
    try
      //DeInit Plugin
      PluginLoader.CallDeInit(i);
    except
    end;
  end;

  //Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

end.
