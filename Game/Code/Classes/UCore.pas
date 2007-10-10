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
      hLoadTextures:    THandle;
      hExitQuery:       THandle;
      hExit:            THandle;
      hDebug:           THandle;
      hError:           THandle;
      sReportError:       THandle;
      sReportDebug:       THandle;

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

      //---------------
      //Main Methods to control the Core:
      //---------------
      Constructor Create(const cName: String; const cVersion: LongWord);

      //Starts Loading and Init Process. Then Runs MainLoop. DeInits on Shutdown
      Procedure Run;

      //--------------
      // Hook and Service Procs:
      //--------------
      Function ShowMessage(wParam, lParam: DWord): integer; //Shows a Message (lParam: PChar Text, wParam: Symbol)
      {Function ShowMessage(wParam, lParam: DWord): integer; //Shows a Message (lParam: PChar Text, wParam: Symbol)
      Function ShowMessage(wParam, lParam: DWord): integer; //Shows a Message (lParam: PChar Text, wParam: Symbol)}
  end;

var
  Core: TCore; 

implementation
{$IFDEF win32}
uses Windows;
{$ENDIF}

//-------------
// Create - Creates Class + Hook and Service Manager
//-------------
Constructor TCore.Create(const cName: String; const cVersion: LongWord);
begin
  Name := cName;
  Version := cVersion;
  CurExecuted := 0;

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
  end
  else
    Self.ShowMessage(CORE_SM_ERROR, Integer(PChar('')));
end;

//-------------
//Called one Time per Frame
//-------------
Function TCore.MainLoop: Boolean;
begin
  Result := True;

end;

//-------------
//Function Get all Modules and Creates them
//-------------
Function TCore.GetModules: Boolean;
var
  I: Integer;
begin
  For I := 0 to high(Modules) do
  begin
    Modules[I].NeedsDeInit := False;
    Modules[I].Module := CORE_MODULES_TO_LOAD[I].Create;
    Modules[I].Module.Info(@Modules[I].Info);
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
    Result := Modules[I].Module.Load;
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
    Result := Modules[I].Module.Init;
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
  hLoadTextures    := Hooks.AddEvent('Core/LoadTextures');
  hExitQuery       := Hooks.AddEvent('Core/ExitQuery');
  hExit            := Hooks.AddEvent('Core/Exit');
  hDebug           := Hooks.AddEvent('Core/NewDebugInfo');
  hError           := Hooks.AddEvent('Core/NewError');
  sReportError     := Services.AddService('Core/ReportError');
  sReportDebug     := Services.AddService('Core/ReportDebug');
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
//Shows a MessageDialog (lParam: PChar Text, wParam: Symbol)
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

end.