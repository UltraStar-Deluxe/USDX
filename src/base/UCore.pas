{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UCore;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  uPluginDefs,
  uCoreModule,
  UHooks,
  UServices,
  UModules;

{*********************
  TCore
  Class manages all CoreModules, the StartUp, the MainLoop and the shutdown process
  Also, it does some error handling, and maybe sometime multithreaded loading ;)
*********************}

type
  TModuleListItem = record
    Module:       TCoreModule; // Instance of the modules class
    Info:         TModuleInfo; // ModuleInfo returned by modules modulinfo proc
    NeedsDeInit:  boolean;     // True if module was succesful inited
  end;
  
  TCore = class
    private
      // Some Hook Handles. See Plugin SDKs Hooks.txt for Infos
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

      Modules:          array [0..High(CORE_MODULES_TO_LOAD)] of TModuleListItem;

      // Cur + Last Executed Setting and Getting ;)
      iCurExecuted: integer;
      iLastExecuted: integer;

      procedure SetCurExecuted(Value: integer);

      // Function Get all Modules and Creates them
      function GetModules: boolean;

      // Loads Core and all Modules
      function Load: boolean;

      // Inits Core and all Modules
      function Init: boolean;

      // DeInits Core and all Modules
      function DeInit: boolean;

      // Load the Core
      function LoadCore: boolean;

      // Init the Core
      function InitCore: boolean;

      // DeInit the Core
      function DeInitCore: boolean;

      // Called one time per frame
      function MainLoop: boolean;

    public
      Hooks:             THookManager;    // The Hook Manager ;)
      Services:          TServiceManager; // The Service Manager

      Name:              string;          // Name of this application
      Version:           LongWord;        // Version of this ". For info look plugindefs functions

      LastErrorReporter: string;          // Who reported the last error string
      LastErrorString:   string;          // Last error string reported

      property CurExecuted:  integer read iCurExecuted write SetCurExecuted;       //ID of plugin or module curently executed
      property LastExecuted: integer read iLastExecuted;

      //---------------
      // Main methods to control the core:
      //---------------
      constructor Create(const cName: string; const cVersion: LongWord);

      // Starts loading and init process. Then runs MainLoop. DeInits on shutdown
      procedure Run;

      // Method for other classes to get pointer to a specific module
      function GetModulebyName(const Name: string): PCoreModule;

      //--------------
      // Hook and service procs:
      //--------------
      function ShowMessage(wParam: TwParam; lParam: TlParam): integer; //Shows a Message (lParam: PChar Text, wParam: Symbol)
      function ReportError(wParam: TwParam; lParam: TlParam): integer; //Shows a Message (wParam: Pchar(Message), lParam: PChar(Reportername))
      function ReportDebug(wParam: TwParam; lParam: TlParam): integer; //Shows a Message (wParam: Pchar(Message), lParam: PChar(Reportername))
      function Retranslate(wParam: TwParam; lParam: TlParam): integer; //Calls Translate hook
      function ReloadTextures(wParam: TwParam; lParam: TlParam): integer; //Calls LoadTextures hook
      function GetModuleInfo(wParam: TwParam; lParam: TlParam): integer; //If lParam = nil then get length of Moduleinfo array. If lparam <> nil then write array of TModuleInfo to address at lparam
      function GetApplicationHandle(wParam: TwParam; lParam: TlParam): integer; //Returns Application Handle
  end;

var
  Core: TCore; 

implementation

uses
  {$IFDEF win32}
  Windows,
  {$ENDIF}
  SysUtils;

//-------------
// Create - Creates Class + Hook and Service Manager
//-------------
constructor TCore.Create(const cName: string; const cVersion: LongWord);
begin
  inherited Create;

  Name := cName;
  Version := cVersion;
  iLastExecuted := 0;
  iCurExecuted := 0;

  LastErrorReporter := '';
  LastErrorString   := '';

  Hooks := THookManager.Create(50);
  Services := TServiceManager.Create;
end;

//-------------
// Starts Loading and Init process. Then runs MainLoop. DeInits on shutdown
//-------------
procedure TCore.Run;
var
  Success: boolean;

  procedure HandleError(const ErrorMsg: string);
  begin
    if (LastErrorString <> '') then
      Self.ShowMessage(CORE_SM_ERROR, PChar(ErrorMsg + ': ' + LastErrorString))
    else
      Self.ShowMessage(CORE_SM_ERROR, PChar(ErrorMsg));

    // DeInit
    DeInit;
  end;

begin
  // Get modules
  try
    Success := GetModules();
  except
    Success := false;
  end;

  if (not Success) then
  begin
    HandleError('Error Getting Modules');
    Exit;
  end;

  // Loading
  try
    Success := Load();
  except
    Success := false;
  end;

  if (not Success) then
  begin
    HandleError('Error loading Modules');
    Exit;
  end;

  // Init
  try
    Success := Init();
  except
    Success := false;
  end;

  if (not Success) then
  begin
    HandleError('Error initing Modules');
    Exit;
  end;

  // Call Translate Hook
  if (Hooks.CallEventChain(hTranslate, 0, nil) <> 0) then
  begin
    HandleError('Error translating');
    Exit;
  end;

  // Calls LoadTextures Hook
  if (Hooks.CallEventChain(hLoadTextures, 0, nil) <> 0) then
  begin
    HandleError('Error loading textures');
    Exit;
  end;
  
  // Calls Loading Finished Hook
  if (Hooks.CallEventChain(hLoadingFinished, 0, nil) <> 0) then
  begin
    HandleError('Error calling LoadingFinished Hook');
    Exit;
  end;
  
  // Start MainLoop
  while Success do
  begin
    Success := MainLoop();
    // to-do : Call Display Draw here
  end;
end;

//-------------
// Called one time per frame
//-------------
function TCore.MainLoop: boolean;
begin
  Result := false;
end;

//-------------
// Function get all modules and creates them
//-------------
function TCore.GetModules: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to high(Modules) do
  begin
    try
      Modules[i].NeedsDeInit := false;
      Modules[i].Module := CORE_MODULES_TO_LOAD[i].Create;
      Modules[i].Module.Info(@Modules[i].Info);
    except
      ReportError(integer(PChar('Can''t get module #' + InttoStr(i) + ' "' + Modules[i].Info.Name + '"')), PChar('Core'));
      Exit;
    end;
  end;
  Result := true;
end;

//-------------
// Loads core and all modules
//-------------
function TCore.Load: boolean;
var
  i: integer;
begin
  Result := LoadCore;

  for i := 0 to High(CORE_MODULES_TO_LOAD) do
  begin
    try
      Result := Modules[i].Module.Load;
    except
      Result := false;
    end;

    if (not Result) then
    begin
      ReportError(integer(PChar('Error loading module #' + InttoStr(i) + ' "' + Modules[i].Info.Name + '"')), PChar('Core'));
      break;
    end;
  end;
end;

//-------------
// Inits core and all modules
//-------------
function TCore.Init: boolean;
var
  i: integer;
begin
  Result := InitCore;

  for i := 0 to High(CORE_MODULES_TO_LOAD) do
  begin
    try
      Result := Modules[i].Module.Init;
    except
      Result := false;
    end;

    if (not Result) then
    begin
      ReportError(integer(PChar('Error initing module #' + InttoStr(i) + ' "' + Modules[i].Info.Name + '"')), PChar('Core'));
      break;
    end;

    Modules[i].NeedsDeInit := Result;
  end;
end;

//-------------
// DeInits core and all modules
//-------------
function TCore.DeInit: boolean;
var
  i: integer;
begin

  for i :=  High(CORE_MODULES_TO_LOAD) downto 0 do
  begin
    try
      if (Modules[i].NeedsDeInit) then
        Modules[i].Module.DeInit;
    except
    end;
  end;

  DeInitCore;

  Result := true;
end;

//-------------
// Load the Core
//-------------
function TCore.LoadCore: boolean;
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

  // A little Test
  Hooks.AddSubscriber('Core/NewError', HookTest);

  result := true;
end;

//-------------
// Init the Core
//-------------
function TCore.InitCore: boolean;
begin
  //Don not init something atm.
  result := true;
end;

//-------------
// DeInit the Core
//-------------
function TCore.DeInitCore: boolean;
begin
  // TODO: write TService-/HookManager. Free and call it here
  Result := true;
end;

//-------------
// Method for other classes to get pointer to a specific module
//-------------
function TCore.GetModuleByName(const Name: string): PCoreModule;
var i: integer;
begin
  Result := nil;
  for i := 0 to High(Modules) do
  begin
    if (Modules[i].Info.Name = Name) then
    begin
      Result := @Modules[i].Module;
      Break;
    end;
  end;
end;

//-------------
// Shows a MessageDialog (lParam: PChar Text, wParam: Symbol)
//-------------
function TCore.ShowMessage(wParam: TwParam; lParam: TlParam): integer;
{$IFDEF MSWINDOWS}
var Params: Cardinal;
{$ENDIF}
begin
  Result := -1;

  {$IFDEF MSWINDOWS}
  if (lParam <> nil) then
  begin
    Params := MB_OK;
    case wParam of
      CORE_SM_ERROR: Params := Params or MB_ICONERROR;
      CORE_SM_WARNING: Params := Params or MB_ICONWARNING;
      CORE_SM_INFO: Params := Params or MB_ICONINFORMATION;
    end;

    // Show:
    Result := Messagebox(0, lParam, PChar(Name), Params);
  end;
  {$ENDIF}

  // TODO: write ShowMessage for other OSes
end;

//-------------
// Calls NewError HookChain (wParam: Pchar(Message), lParam: PChar(Reportername))
//-------------
function TCore.ReportError(wParam: TwParam; lParam: TlParam): integer;
begin
  //Update LastErrorReporter and LastErrorString
  LastErrorReporter := string(PChar(lParam));
  LastErrorString   := string(PChar(Pointer(wParam)));
  
  Hooks.CallEventChain(hError, wParam, lParam);

  // FIXME: return a correct result
  Result := 0;
end;

//-------------
// Calls NewDebugInfo HookChain (wParam: Pchar(Message), lParam: PChar(Reportername))
//-------------
function TCore.ReportDebug(wParam: TwParam; lParam: TlParam): integer;
begin
  Hooks.CallEventChain(hDebug, wParam, lParam);

  // FIXME: return a correct result
  Result := 0;
end;

//-------------
// Calls Translate hook
//-------------
function TCore.Retranslate(wParam: TwParam; lParam: TlParam): integer;
begin
  Hooks.CallEventChain(hTranslate, 1, nil);

  // FIXME: return a correct result
  Result := 0;
end;

//-------------
// Calls LoadTextures hook
//-------------
function TCore.ReloadTextures(wParam: TwParam; lParam: TlParam): integer;
begin
  Hooks.CallEventChain(hLoadTextures, 1, nil);

  // FIXME: return a correct result
  Result := 0;
end;

//-------------
// If lParam = nil then get length of Moduleinfo array. If lparam <> nil then write array of TModuleInfo to address at lparam
//-------------
function TCore.GetModuleInfo(wParam: TwParam; lParam: TlParam): integer;
var
  I: integer;
begin
  if (Pointer(lParam) = nil) then
  begin
    Result := Length(Modules);
  end
  else
  begin
    try
      for I := 0 to High(Modules) do
      begin
        AModuleInfo(Pointer(lParam))[I].Name := Modules[I].Info.Name;
        AModuleInfo(Pointer(lParam))[I].Version := Modules[I].Info.Version;
        AModuleInfo(Pointer(lParam))[I].Description := Modules[I].Info.Description;
      end;
      Result := Length(Modules);
    except
      Result := -1;
    end;
  end;
end;

//-------------
// Returns Application Handle
//-------------
function TCore.GetApplicationHandle(wParam: TwParam; lParam: TlParam): integer;
begin
  Result := hInstance;
end;

//-------------
// Called when setting CurExecuted
//-------------
procedure TCore.SetCurExecuted(Value: integer);
begin
  // Set Last Executed
  iLastExecuted := iCurExecuted;

  // Set Cur Executed
  iCurExecuted := Value;
end;

end.
