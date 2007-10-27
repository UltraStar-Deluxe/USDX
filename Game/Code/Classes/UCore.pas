unit UCore;

interface
uses uPluginDefs, uCoreModule, UHooks, UServices, uModules;
{*********************
  TCore
  Class manages all CoreModules, teh StartUp, teh MainLoop and the shutdown process
  Also it does some Error Handling, and maybe sometime multithreaded Loading ;)
*********************}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TModuleListItem = record
    Module:       TCoreModule; //Instance of the Modules Class
    Info:         TModuleInfo; //ModuleInfo returned by Modules Modulinfo Proc
    NeedsDeInit:  Boolean;     //True if Module was succesful inited
  end;
  
  TCore = class
    private
      //Some Hook Handles. See Plugin SDKs Hooks.txt for Infos
      hLoadingFinished: THandle;
      hMainLoop:        THandle;
      hTranslate:       THandle;
      hLoadTextures:    THandle;
      hExitQuery:       THandle;
      hExit:            THandle;
      hDebug:           THandle;
      hError:           THandle;
      sReportError:     THandle;
      sReportDebug:     THandle;
      sShowMessage:     THandle;
      sRetranslate:     THandle;
      sReloadTextures:  THandle;
      sGetModuleInfo:   THandle;
      sGetApplicationHandle: THandle;

      Modules:          Array [0..High(CORE_MODULES_TO_LOAD)] of TModuleListItem;

      //Function Get all Modules and Creates them
      Function GetModules: Boolean;

      //Loads Core and all Modules
      Function Load: Boolean;

      //Inits Core and all Modules
      Function Init: Boolean;

      //DeInits Core and all Modules
      Function DeInit: Boolean;

      //Load the Core
      Function LoadCore: Boolean;

      //Init the Core
      Function InitCore: Boolean;

      //DeInit the Core
      Function DeInitCore: Boolean;

      //Called one Time per Frame
      Function MainLoop: Boolean;

    public
      Hooks:            THookManager;   //Teh Hook Manager ;)
      Services:         TServiceManager;//The Service Manager

      CurExecuted:      Integer;        //ID of Plugin or Module curently Executed

      Name:             String;         //Name of this Application
      Version:          LongWord;       //Version of this ". For Info Look PluginDefs Functions

      LastErrorReporter:String;         //Who Reported the Last Error String
      LastErrorString:  String;         //Last Error String reported


      //---------------
      //Main Methods to control the Core:
      //---------------
      Constructor Create(const cName: String; const cVersion: LongWord);

      //Starts Loading and Init Process. Then Runs MainLoop. DeInits on Shutdown
      Procedure Run;

      //Method for other Classes to get Pointer to a specific Module
      Function GetModulebyName(const Name: String): PCoreModule;

      //--------------
      // Hook and Service Procs:
      //--------------
      Function ShowMessage(wParam, lParam: DWord): integer; //Shows a Message (lParam: PChar Text, wParam: Symbol)
      Function ReportError(wParam, lParam: DWord): integer; //Shows a Message (wParam: Pchar(Message), lParam: PChar(Reportername))
      Function ReportDebug(wParam, lParam: DWord): integer; //Shows a Message (wParam: Pchar(Message), lParam: PChar(Reportername))
      Function Retranslate(wParam, lParam: DWord): integer; //Calls Translate hook
      Function ReloadTextures(wParam, lParam: DWord): integer; //Calls LoadTextures hook
      Function GetModuleInfo(wParam, lParam: DWord): integer; //If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TModuleInfo to address at lparam
      Function GetApplicationHandle(wParam, lParam: DWord): integer; //Returns Application Handle
  end;

var
  Core: TCore; 

implementation
uses SysUtils,
{$IFDEF win32}
Windows
{$ENDIF};

//-------------
// Create - Creates Class + Hook and Service Manager
//-------------
Constructor TCore.Create(const cName: String; const cVersion: LongWord);
begin
  Name := cName;
  Version := cVersion;
  CurExecuted := 0;

  LastErrorReporter := '';
  LastErrorString   := '';

  Hooks := THookManager.Create(50);
  Services := TServiceManager.Create;
end;

//-------------
//Starts Loading and Init Process. Then Runs MainLoop. DeInits on Shutdown
//-------------
Procedure TCore.Run;
var
  noError: Boolean;
begin
  //Get Modules
  Try
    noError := GetModules;
  Except
    noError := False;
  end;

  //Loading
  if (noError) then
  begin
    Try
      noError := Load;
    Except
      noError := False;
    end;

    if (noError) then
    begin //Init
      Try
        noError := Init;
      Except
        noError := False;
      end;

      If noError then
      begin
        //Call Translate Hook
        noError := (Hooks.CallEventChain(hTranslate, 0, 0) = 0);

        If noError then
        begin //Calls LoadTextures Hook
          noError := (Hooks.CallEventChain(hLoadTextures, 0, 0) = 0);

          if noError then
          begin //Calls Loading Finished Hook
            noError := (Hooks.CallEventChain(hLoadingFinished, 0, 0) = 0);

            If noError then
            begin
              //Start MainLoop
              While noError do
              begin
                noError := MainLoop;
                // to-do : Call Display Draw here
              end;
            end
            else
            begin
              If (LastErrorString <> '') then
                Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error calling LoadingFinished Hook: ' + LastErrorString)))
              else
                Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error calling LoadingFinished Hook')));
            end;
          end
          else
          begin
            If (LastErrorString <> '') then
              Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error loading textures: ' + LastErrorString)))
            else
              Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error loading textures')));
          end;
        end
        else
        begin
          If (LastErrorString <> '') then
            Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error translating: ' + LastErrorString)))
          else
            Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error translating')));
        end;
        
      end
      else
      begin
        If (LastErrorString <> '') then
          Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error initing Modules: ' + LastErrorString)))
        else
          Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error initing Modules')));
      end;
    end
    else
    begin
      If (LastErrorString <> '') then
        Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error loading Modules: ' + LastErrorString)))
      else
        Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error loading Modules')));
    end;
  end
  else
  begin
    If (LastErrorString <> '') then
      Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error Getting Modules: ' + LastErrorString)))
    else
      Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('Error Getting Modules')));
  end;

  //DeInit
  DeInit;
end;

//-------------
//Called one Time per Frame
//-------------
Function TCore.MainLoop: Boolean;
begin
  Result := False;

end;

//-------------
//Function Get all Modules and Creates them
//-------------
Function TCore.GetModules: Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    For I := 0 to high(Modules) do
    begin
      Modules[I].NeedsDeInit := False;
      Modules[I].Module := CORE_MODULES_TO_LOAD[I].Create;
      Modules[I].Module.Info(@Modules[I].Info);
    end;
    Result := True;
  except
    ReportError(Integer(PChar('Can''t get module #' + InttoStr(I) + ' "' + Modules[I].Info.Name + '"')), Integer(PChar('Core')));
  end;
end;

//-------------
//Loads Core and all Modules
//-------------
Function TCore.Load: Boolean;
var
  I: Integer;
begin
  Result := LoadCore;

  I := 0;
  While ((Result = True) AND (I <= High(CORE_MODULES_TO_LOAD))) do
  begin
    try
      Result := Modules[I].Module.Load;
    except
      Result := False;
      ReportError(Integer(PChar('Error loading module #' + InttoStr(I) + ' "' + Modules[I].Info.Name + '"')), Integer(PChar('Core')));
    end;

    Inc(I);
  end;
end;

//-------------
//Inits Core and all Modules
//-------------
Function TCore.Init: Boolean;
var
  I: Integer;
begin
  Result := InitCore;

  I := 0;
  While ((Result = True) AND (I <= High(CORE_MODULES_TO_LOAD))) do
  begin
    try
      Result := Modules[I].Module.Init;
    except
      Result := False;
      ReportError(Integer(PChar('Error initing module #' + InttoStr(I) + ' "' + Modules[I].Info.Name + '"')), Integer(PChar('Core')));
    end;

    Modules[I].NeedsDeInit := Result;
    Inc(I);
  end;
end;

//-------------
//DeInits Core and all Modules
//-------------
Function TCore.DeInit: Boolean;
var
  I: Integer;
label Continue;
begin
  I := High(CORE_MODULES_TO_LOAD);

  Continue:
  Try
    While (I >= 0) do
    begin
      If (Modules[I].NeedsDeInit) then
        Modules[I].Module.DeInit;

      Dec(I);
    end;
  Except


  end;
  If (I >= 0) then
    GoTo Continue;

  DeInitCore;
end;

//-------------
//Load the Core
//-------------
Function TCore.LoadCore: Boolean;
begin
  hLoadingFinished := Hooks.AddEvent('Core/LoadingFinished');
  hMainLoop        := Hooks.AddEvent('Core/MainLoop');
  hTranslate       := Hooks.AddEvent('Core/Translate');
  hLoadTextures    := Hooks.AddEvent('Core/LoadTextures');
  hExitQuery       := Hooks.AddEvent('Core/ExitQuery');
  hExit            := Hooks.AddEvent('Core/Exit');
  hDebug           := Hooks.AddEvent('Core/NewDebugInfo');
  hError           := Hooks.AddEvent('Core/NewError');
  
  sReportError     := Services.AddService('Core/ReportError', nil, Self.ReportError);
  sReportDebug     := Services.AddService('Core/ReportDebug', nil, Self.ReportDebug);
  sShowMessage     := Services.AddService('Core/ShowMessage', nil, Self.ShowMessage);
  sRetranslate     := Services.AddService('Core/Retranslate', nil, Self.Retranslate);
  sReloadTextures  := Services.AddService('Core/ReloadTextures', nil, Self.ReloadTextures);
  sGetModuleInfo   := Services.AddService('Core/GetModuleInfo', nil, Self.GetModuleInfo);
  sGetApplicationHandle := Services.AddService('Core/GetApplicationHandle', nil, Self.GetApplicationHandle);

  //A little Test
  Hooks.AddSubscriber('Core/NewError', HookTest);
end;

//-------------
//Init the Core
//-------------
Function TCore.InitCore: Boolean;
begin
  //Dont Init s.th. atm.
end;

//-------------
//DeInit the Core
//-------------
Function TCore.DeInitCore: Boolean;
begin


  // to-do : write TService-/HookManager.Free and call it here
end;

//-------------
//Method for other Classes to get Pointer to a specific Module
//-------------
Function TCore.GetModulebyName(const Name: String): PCoreModule;
var I: Integer;
begin
  Result := nil;
  For I := 0 to high(Modules) do
    If (Modules[I].Info.Name = Name) then
    begin
      Result := @Modules[I].Module;
      Break;
    end;
end;

//-------------
// Shows a MessageDialog (lParam: PChar Text, wParam: Symbol)
//-------------
Function TCore.ShowMessage(wParam, lParam: DWord): integer;
var Params: Cardinal; 
begin
  Result := -1;

  {$IFDEF win32}
  If (ptr(lParam)<>nil) then
  begin
    Params := MB_OK;
    Case wParam of
      CORE_SM_ERROR: Params := Params or MB_ICONERROR;
      CORE_SM_WARNING: Params := Params or MB_ICONWARNING;
      CORE_SM_INFO: Params := Params or MB_ICONINFORMATION;
    end;

    //Anzeigen:
    Result := Messagebox(0, ptr(lParam), PChar(Name), Params);
  end;
  {$ENDIF}

  // to-do : write ShowMessage for other OSes
end;

//-------------
// Calls NewError HookChain (wParam: Pchar(Message), lParam: PChar(Reportername))
//-------------
Function TCore.ReportError(wParam, lParam: DWord): integer;
begin
  //Update LastErrorReporter and LastErrorString
  LastErrorReporter := String(PChar(Ptr(lParam)));
  LastErrorString   := String(PChar(Ptr(wParam)));
  
  Hooks.CallEventChain(hError, wParam, lParam);
end;

//-------------
// Calls NewDebugInfo HookChain (wParam: Pchar(Message), lParam: PChar(Reportername))
//-------------
Function TCore.ReportDebug(wParam, lParam: DWord): integer;
begin
  Hooks.CallEventChain(hDebug, wParam, lParam);
end;

//-------------
// Calls Translate hook
//-------------
Function TCore.Retranslate(wParam, lParam: DWord): integer;
begin
  Hooks.CallEventChain(hTranslate, 0, 1);
end;

//-------------
// Calls LoadTextures hook
//-------------
Function TCore.ReloadTextures(wParam, lParam: DWord): integer;
begin
  Hooks.CallEventChain(hLoadTextures, 0, 1);
end;

//-------------
// If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TModuleInfo to address at lparam
//-------------
Function TCore.GetModuleInfo(wParam, lParam: DWord): integer;
begin
  if (ptr(lParam) = nil) then
  begin
    Result := Length(Modules);
  end
  else
  begin
    Try
      For Result := 0 to High(Modules) do
      begin
        AModuleInfo(ptr(lParam))[Result].Name := Modules[Result].Info.Name;
        AModuleInfo(ptr(lParam))[Result].Version := Modules[Result].Info.Version;
        AModuleInfo(ptr(lParam))[Result].Description := Modules[Result].Info.Description;
      end;
    Except
      Result := -1;
    end;
  end;
end;

//-------------
// Returns Application Handle
//-------------
Function TCore.GetApplicationHandle(wParam, lParam: DWord): integer;
begin
  Result := hInstance;
end;

end.