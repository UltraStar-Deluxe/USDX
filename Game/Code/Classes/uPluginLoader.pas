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

uses UPluginDefs, UCoreModule;

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

      Procedure FreePlugin(Index: Cardinal);
    public
      PluginInterface: TUS_PluginInterface;
      Plugins: Array of TPluginListItem;

      //TCoreModule methods to inherit
      Constructor Create; override;
      Procedure Info(const pInfo: PModuleInfo); override;
      Function Load: Boolean; override;
      Function Init: Boolean; override;
      Procedure DeInit; override;
      Destructor Destroy; override;

      //New Methods
      Procedure BrowseDir(Path: String);             //Browses the Path at _Path_ for Plugins
      Function  PluginExists(Name: String): Integer; //If Plugin Exists: Index of Plugin, else -1
      Procedure AddPlugin(Filename: String);//Adds Plugin to the Array

      Function  CallLoad(Index: Cardinal): Integer;
      Function  CallInit(Index: Cardinal): Integer;
      Procedure CallDeInit(Index: Cardinal);

      //Services offered
      Function LoadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      Function UnloadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      Function GetPluginInfo(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) Else (Get PluginInfo of Plugin with Index(wParam) to Address at lParam)
      Function GetPluginState(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) Else (Return PluginInfo of Plugin with Index(wParam))

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
      Constructor Create; override;
      
      Procedure Info(const pInfo: PModuleInfo); override;
      Function Load: Boolean; override;
      Function Init: Boolean; override;
      Procedure DeInit; override;
  end;

const
  {$IFDEF MSWINDOWS}
  PluginFileExtension = '.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  PluginFileExtension = '.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  PluginFileExtension = '.dylib';
  {$ENDIF}

implementation
uses UCore, UPluginInterface,
{$IFDEF MSWINDOWS}
  windows,
{$ELSE}
  dynlibs,
{$ENDIF}
UMain, SysUtils;

{*********************
  TPluginLoader
  Implentation
*********************}

//-------------
// Function that gives some Infos about the Module to the Core
//-------------
Procedure TPluginLoader.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TPluginLoader';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Searches for Plugins, loads and unloads them';
end;

//-------------
// Just the Constructor
//-------------
Constructor TPluginLoader.Create;
begin
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
Function TPluginLoader.Load: Boolean;
begin
  Result := True;

  Try
    //Start Searching for Plugins
    BrowseDir(PluginPath);
  Except
    Result := False;
    Core.ReportError(Integer(PChar('Error Browsing and Loading.')), PChar('TPluginLoader'));
  end;
end;

//-------------
//Is Called on Init Process
//In this Method you can Hook some Events and Create + Init
//your Classes, Variables etc.
//If False is Returned this will cause a Forced Exit
//-------------
Function TPluginLoader.Init: Boolean;
begin
  //Just set Prvate Var to true.
  LoadingProcessFinished := True;
  Result := True;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
Procedure TPluginLoader.DeInit;
var
  I: Integer;
begin
  //Force DeInit
  //If some Plugins aren't DeInited for some Reason o0
  For I := 0 to High(Plugins) do
  begin
    If (Plugins[I].State < 4) then
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
end;

//--------------
// Browses the Path at _Path_ for Plugins
//--------------
Procedure TPluginLoader.BrowseDir(Path: String);
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
Function  TPluginLoader.PluginExists(Name: String): Integer;
var
  I: Integer;
begin
  Result := -1;

  If (Length(Name) <= 32 { =>Length(TUS_PluginInfo.Name)}) then
  begin
    For I := 0 to High(Plugins) do
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
Procedure TPluginLoader.AddPlugin(Filename: String);
var
  hLib: THandle;
  PInfo: Proc_PluginInfo;
  Info: TUS_PluginInfo;
  PluginID: Integer;
begin
  If (FileExists(Filename)) then
  begin //Load Libary
    hLib := LoadLibrary(PChar(Filename));
    If (hLib <> 0) then
    begin //Try to get Address of the Info Proc
      PInfo := GetProcAddress (hLib, PChar('USPlugin_Info'));
      If (@PInfo <> nil) then
      begin
        Info.cbSize := SizeOf(TUS_PluginInfo);

        Try //Call Info Proc
          PInfo(@Info);
        Except
          Info.Name := '';
          Core.ReportError(Integer(PChar('Error getting Plugin Info: ' + Filename)), PChar('TPluginLoader'));
        end;

        //Is Name set ?
        If (Trim(Info.Name) <> '') then
        begin
          PluginID := PluginExists(Info.Name);

          If (PluginID > 0) AND (Plugins[PluginID].State >=4) then
            PluginID := -1;

          If (PluginID = -1) then
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

            If (@Plugins[PluginID].Procs.Load = nil) OR (@Plugins[PluginID].Procs.Init = nil) OR (@Plugins[PluginID].Procs.DeInit = nil) then
            begin
              Plugins[PluginID].State := 255;
              FreeLibrary(hLib);
              Core.ReportError(Integer(PChar('Can''t get Plugin Procs from Libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
            end;

            //Emulate loading process if this Plugin is loaded to late
            If (LoadingProcessFinished) then
            begin
              CallLoad(PluginID);
              CallInit(PluginID);
            end;
          end
          Else If (LoadingProcessFinished = False) then
          begin
            If (Plugins[PluginID].Info.Version < Info.Version) then
            begin //Found newer Version of this Plugin
              Core.ReportDebug(Integer(PChar('Found a newer Version of Plugin: ' + String(Info.Name))), PChar('TPluginLoader'));

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

              If (@Plugins[PluginID].Procs.Load = nil) OR (@Plugins[PluginID].Procs.Init = nil) OR (@Plugins[PluginID].Procs.DeInit = nil) then
              begin
                FreeLibrary(hLib);
                Plugins[PluginID].State := 255;
                Core.ReportError(Integer(PChar('Can''t get Plugin Procs from Libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
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
            Core.ReportError(Integer(PChar('Plugin with this Name already exists: ' + String(Info.Name))), PChar('TPluginLoader'));
          end;
        end
        else
        begin
          FreeLibrary(hLib);
          Core.ReportError(Integer(PChar('No name reported: ' + Filename)), PChar('TPluginLoader'));
        end;
      end
      else
      begin
        FreeLibrary(hLib);
        Core.ReportError(Integer(PChar('Can''t find Info Procedure: ' + Filename)), PChar('TPluginLoader'));
      end;
    end
    else
      Core.ReportError(Integer(PChar('Can''t load Plugin Libary: ' + Filename)), PChar('TPluginLoader'));
  end;
end;

//--------------
// Calls Load Func of Plugin with the given Index
//--------------
Function  TPluginLoader.CallLoad(Index: Cardinal): Integer;
begin
  Result := -2;
  If(Index < Length(Plugins)) then
  begin
    If (@Plugins[Index].Procs.Load <> nil) AND (Plugins[Index].State = 0) then
    begin
      Try
        Result := Plugins[Index].Procs.Load(@PluginInterface);
      Except
        Result := -3;
      End;

      If (Result = 0) then
        Plugins[Index].State := 1
      Else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(Integer(PChar('Error calling Load Function from Plugin: ' + String(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls Init Func of Plugin with the given Index
//--------------
Function  TPluginLoader.CallInit(Index: Cardinal): Integer;
begin
  Result := -2;
  If(Index < Length(Plugins)) then
  begin
    If (@Plugins[Index].Procs.Init <> nil) AND (Plugins[Index].State = 1) then
    begin
      Try
        Result := Plugins[Index].Procs.Init(@PluginInterface);
      Except
        Result := -3;
      End;
      
      If (Result = 0) then
      begin
        Plugins[Index].State := 2;
        Plugins[Index].NeedsDeInit := True;
      end
      Else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(Integer(PChar('Error calling Init Function from Plugin: ' + String(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls DeInit Proc of Plugin with the given Index
//--------------
Procedure TPluginLoader.CallDeInit(Index: Cardinal);
begin
  If(Index < Length(Plugins)) then
  begin
    If (Plugins[Index].State < 4) then
    begin
      If (@Plugins[Index].Procs.DeInit <> nil) and (Plugins[Index].NeedsDeInit) then
        Try
          Plugins[Index].Procs.DeInit(@PluginInterface);
        Except

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
Procedure TPluginLoader.FreePlugin(Index: Cardinal);
begin
  Plugins[Index].State := 4;
  Plugins[Index].Procs.Load := nil;
  Plugins[Index].Procs.Init := nil;
  Plugins[Index].Procs.DeInit := nil;

  If (Plugins[Index].hLib <> 0) then
    FreeLibrary(Plugins[Index].hLib);
end;



//--------------
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the Plugin
//--------------
Function TPluginLoader.LoadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: Integer;
  sFile: String;
begin
  Result := -1;
  sFile := '';
  //lParam is ID
  If (lParam = nil) then
  begin
    Index := wParam;
  end
  else
  begin //lParam is PChar
    try
      sFile := String(PChar(lParam));
      Index := PluginExists(sFile);
      If (Index < 0) And FileExists(sFile) then
      begin //Is Filename
        AddPlugin(sFile);
        Result := Plugins[High(Plugins)].State;
      end;
    except
      Index := -2;
    end;
  end;


  If (Index >= 0) and (Index < Length(Plugins)) then
  begin
    AddPlugin(Plugins[Index].Path);
    Result := Plugins[Index].State;
  end;
end;

//--------------
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the Plugin
//--------------
Function TPluginLoader.UnloadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: Integer;
  sName: String;
begin
  Result := -1;
  //lParam is ID
  If (lParam = nil) then
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


  If (Index >= 0) and (Index < Length(Plugins)) then
      CallDeInit(Index)
end;

//--------------
// If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) Else (Get PluginInfo of Plugin with Index(wParam) to Address at lParam)
//--------------
Function TPluginLoader.GetPluginInfo(wParam: TwParam; lParam: TlParam): integer;
var I: Integer;
begin
  Result := 0;
  If (wParam > 0) then
  begin //Get Info of 1 Plugin
    If (lParam <> nil) AND (wParam < Length(Plugins)) then
    begin
      Try
        Result := 1;
        PUS_PluginInfo(lParam)^ := Plugins[wParam].Info;
      Except

      End;
    end;
  end
  Else If (lParam = nil) then
  begin //Get Length of Plugin (Info) Array
    Result := Length(Plugins);
  end
  Else //Write PluginInfo Array to Address in lParam
  begin
    Try
      For I := 0 to high(Plugins) do
        PAUS_PluginInfo(lParam)^[I] := Plugins[I].Info;
      Result := Length(Plugins);
    Except
      Core.ReportError(Integer(PChar('Could not write PluginInfo Array')), PChar('TPluginLoader'));
    End;
  end;

end;

//--------------
// If wParam = -1 then (If lParam = nil then get length of Plugin State Array. If lparam <> nil then write array of Byte to address at lparam) Else (Return State of Plugin with Index(wParam))
//--------------
Function TPluginLoader.GetPluginState(wParam: TwParam; lParam: TlParam): integer;
var I: Integer;
begin
  Result := -1;
  If (wParam > 0) then
  begin //Get State of 1 Plugin
    If (wParam < Length(Plugins)) then
    begin
      Result := Plugins[wParam].State;
    end;
  end
  Else If (lParam = nil) then
  begin //Get Length of Plugin (Info) Array
    Result := Length(Plugins);
  end
  Else //Write PluginInfo Array to Address in lParam
  begin
    Try
      For I := 0 to high(Plugins) do
        Byte(Pointer(Integer(lParam) + I)^) := Plugins[I].State;
      Result := Length(Plugins);
    Except
      Core.ReportError(Integer(PChar('Could not write PluginState Array')), PChar('TPluginLoader'));
    End;
  end;
end;





{*********************
  TtehPlugins
  Implentation
*********************}

//-------------
// Function that gives some Infos about the Module to the Core
//-------------
Procedure TtehPlugins.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TtehPlugins';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Module executing the Plugins!';
end;

//-------------
// Just the Constructor
//-------------
Constructor TtehPlugins.Create;
begin
  PluginLoader := nil;
end;

//-------------
//Is Called on Loading.
//In this Method only Events and Services should be created
//to offer them to other Modules or Plugins during the Init process
//If False is Returned this will cause a Forced Exit
//-------------
Function TtehPlugins.Load: Boolean;
var
  I: Integer; //Counter
  CurExecutedBackup: Integer; //backup of Core.CurExecuted Attribute
label Continue;
begin
  //Get Pointer to PluginLoader
  PluginLoader := PPluginLoader(Core.GetModulebyName('TPluginLoader'));
  If (PluginLoader = nil) then
  begin
    Result := False;
    Core.ReportError(Integer(PChar('Could not get Pointer to PluginLoader')), PChar('TtehPlugins'));
  end
  else
  begin
    Result := True;

    //Backup CurExecuted
    CurExecutedBackup := Core.CurExecuted;

    //Start Loading the Plugins
    I := 0;
    Continue:
    Try
      While (I <= High(PluginLoader.Plugins)) do
      begin
        Core.CurExecuted := -1 - I;

        //Unload Plugin if not correctly Executed
        If (PluginLoader.CallLoad(I) <> 0) then
        begin
          PluginLoader.CallDeInit(I);
          PluginLoader.Plugins[I].State := 254; //Plugin asks for unload
          Core.ReportDebug(Integer(PChar('Plugin Selfabort during loading process: ' + String(PluginLoader.Plugins[I].Info.Name))), PChar('TtehPlugins'));
        end
        else
          Core.ReportDebug(Integer(PChar('Plugin loaded succesful: ' + String(PluginLoader.Plugins[I].Info.Name))), PChar('TtehPlugins'));

        Inc(I);
      end;
    Except
      //Plugin could not be loaded.
      // => Show Error Message, then ShutDown Plugin
      on E: Exception do
      begin
        PluginLoader.CallDeInit(I);
        PluginLoader.Plugins[I].State := 255; //Plugin causes Error
        Core.ReportError(Integer(PChar('Plugin causes Error during loading process: ' + PluginLoader.Plugins[I].Info.Name + ', ErrorMsg: "' + E.Message + '"')), PChar('TtehPlugins'));


        //don't forget to increase I
        Inc(I);
      end;
    End;

    If (I <= High(PluginLoader.Plugins)) then
      Goto Continue;

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
Function TtehPlugins.Init: Boolean;
var
  I: Integer; //Counter
  CurExecutedBackup: Integer; //backup of Core.CurExecuted Attribute
label Continue;
begin
  Result := True;

  //Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  //Start Loading the Plugins
  I := 0;
  Continue:
  Try
    While (I <= High(PluginLoader.Plugins)) do
    begin
      Core.CurExecuted := -1 - I;

      //Unload Plugin if not correctly Executed
      If (PluginLoader.CallInit(I) <> 0) then
      begin
        PluginLoader.CallDeInit(I);
        PluginLoader.Plugins[I].State := 254; //Plugin asks for unload
        Core.ReportDebug(Integer(PChar('Plugin Selfabort during init process: ' + String(PluginLoader.Plugins[I].Info.Name))), PChar('TtehPlugins'));
      end
      else
        Core.ReportDebug(Integer(PChar('Plugin inited succesful: ' + String(PluginLoader.Plugins[I].Info.Name))), PChar('TtehPlugins'));

      //don't forget to increase I
      Inc(I);
    end;
  Except
    //Plugin could not be loaded.
    // => Show Error Message, then ShutDown Plugin
    PluginLoader.CallDeInit(I);
    PluginLoader.Plugins[I].State := 255; //Plugin causes Error
    Core.ReportError(Integer(PChar('Plugin causes Error during init process: ' + String(PluginLoader.Plugins[I].Info.Name))), PChar('TtehPlugins'));

    //don't forget to increase I
    Inc(I);
  End;

  If (I <= High(PluginLoader.Plugins)) then
    GoTo Continue;

  //Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
Procedure TtehPlugins.DeInit;
var
  I: Integer; //Counter
  CurExecutedBackup: Integer; //backup of Core.CurExecuted Attribute
label Continue;
begin
  //Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  //Start Loop
  I := 0;
  
  Continue:
  Try
    While (I <= High(PluginLoader.Plugins)) do
    begin
      //DeInit Plugin
      PluginLoader.CallDeInit(I);

      Inc(I);
    end;
  Except
    Inc(I);
  End;

  If I <= High(PluginLoader.Plugins) then
    Goto Continue;

  //Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

end.
