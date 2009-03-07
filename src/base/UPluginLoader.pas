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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/uPluginLoader.pas $
 * $Id: uPluginLoader.pas 1403 2008-09-23 21:17:22Z k-m_schindler $
 *}

unit UPluginLoader;
{*********************
  UPluginLoader
  Unit contains two classes
    TPluginLoader: Class searching for and loading the plugins
    TtehPlugins:   Class representing the plugins in modules chain
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UPluginDefs,
  UCoreModule,
  UPath;

type
  TPluginListItem = record
    Info: TUS_PluginInfo;
    State: byte;          // State of this plugin: 0 - undefined; 1 - loaded; 2 - inited / running; 4 - unloaded; 254 - loading aborted by plugin; 255 - unloaded because of error
    Path: string;         // path to this plugin
    NeedsDeInit: boolean; // if this is inited correctly this should be true
    hLib: THandle;        // handle of loaded libary
    Procs: record         // procs offered by plugin. Don't call this directly use wrappers of TPluginLoader
      Load:   Func_Load;
      Init:   Func_Init;
      DeInit: Proc_DeInit;
    end;
  end;
  {*********************
    TPluginLoader
    Class searches for plugins and manages loading and unloading
  *********************}
  PPluginLoader = ^TPluginLoader;
  TPluginLoader = class (TCoreModule)
    private
      LoadingProcessFinished: boolean;
      sUnloadPlugin:    THandle;
      sLoadPlugin:      THandle;
      sGetPluginInfo:   THandle;
      sGetPluginState:  THandle;

      procedure FreePlugin(Index: integer);
    public
      PluginInterface: TUS_PluginInterface;
      Plugins: array of TPluginListItem;

      // TCoreModule methods to inherit
      constructor Create; override;
      procedure Info(const pInfo: PModuleInfo); override;
      function Load: boolean; override;
      function Init: boolean; override;
      procedure DeInit; override;
      Destructor Destroy; override;

      // New methods
      procedure BrowseDir(Path: string);             // browses the path at _Path_ for plugins
      function  PluginExists(Name: string): integer; // if plugin exists: Index of plugin, else -1
      procedure AddPlugin(Filename: string);         // adds plugin to the array

      function  CallLoad(Index: integer): integer;
      function  CallInit(Index: integer): integer;
      procedure CallDeInit(Index: integer);

      //Services offered
      function LoadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      function UnloadPlugin(wParam: TwParam; lParam: TlParam): integer; //wParam PChar(PluginName/PluginPath) | lParam (if wParam = nil) ID of the Plugin
      function GetPluginInfo(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Get PluginInfo of Plugin with Index(wParam) to Address at lParam)
      function GetPluginState(wParam: TwParam; lParam: TlParam): integer; //If wParam = -1 then (If lParam = nil then get length of Moduleinfo Array. If lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Return PluginInfo of Plugin with Index(wParam))

  end;

  {*********************
    TtehPlugins
    Class represents the plugins in module chain.
    It calls the plugins procs and funcs
  *********************}
  TtehPlugins = class (TCoreModule)
    private
      PluginLoader: PPluginLoader;
    public
      // TCoreModule methods to inherit
      constructor Create; override;

      procedure Info(const pInfo: PModuleInfo); override;
      function Load: boolean; override;
      function Init: boolean; override;
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
  Implementation
*********************}

//-------------
// function that gives some infos about the module to the core
//-------------
procedure TPluginLoader.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TPluginLoader';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Searches for plugins, loads and unloads them';
end;

//-------------
// Just the constructor
//-------------
constructor TPluginLoader.Create;
begin
  inherited;

  // Init PluginInterface
  // Using methods from UPluginInterface
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

  // UnSet private var
  LoadingProcessFinished := false;
end;

//-------------
// Is called on loading.
// In this method only events and services should be created
// to offer them to other modules or plugins during the init process
// if false is returned this will cause a forced exit
//-------------
function TPluginLoader.Load: boolean;
begin
  Result := true;

  try
    // Start searching for plugins
    BrowseDir(PluginPath);
  except
    Result := false;
    Core.ReportError(integer(PChar('Error browsing and loading.')), PChar('TPluginLoader'));
  end;
end;

//-------------
// Is called on init process
// In this method you can hook some events and create + init
// your classes, variables etc.
// If false is returned this will cause a forced exit
//-------------
function TPluginLoader.Init: boolean;
begin
  // Just set private var to true.
  LoadingProcessFinished := true;
  Result := true;
end;

//-------------
// Is called if this module has been inited and there is a exit.
// Deinit is in backwards initing order
//-------------
procedure TPluginLoader.DeInit;
var
  I: integer;
begin
  // Force deinit
  // if some plugins aren't deinited for some reason o0
  for I := 0 to High(Plugins) do
  begin
    if (Plugins[I].State < 4) then
      FreePlugin(I);
  end;

  // Nothing to do here. Core will remove the hooks
end;

//-------------
// Is called if this module will be unloaded and has been created
// Should be used to free memory
//-------------
Destructor TPluginLoader.Destroy;
begin
  // Just save some memory if it wasn't done now..
  SetLength(Plugins, 0);
  inherited;
end;

//--------------
//  Browses the path at _Path_ for plugins
//--------------
procedure TPluginLoader.BrowseDir(Path: string);
var
  SR: TSearchRec;
begin
  // Search for other dirs to browse
  if FindFirst(Path + '*', faDirectory, SR) = 0 then begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        BrowseDir(Path + Sr.Name + PathDelim);
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);

  // Search for plugins at path
  if FindFirst(Path + '*' + PluginFileExtension, 0, SR) = 0 then
  begin
    repeat
      AddPlugin(Path + SR.Name);
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);
end;

//--------------
// If plugin exists: Index of plugin, else -1
//--------------
function  TPluginLoader.PluginExists(Name: string): integer;
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
// Adds plugin to the array
//--------------
procedure TPluginLoader.AddPlugin(Filename: string);
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
    begin // Try to get address of the info proc
      PInfo := GetProcAddress (hLib, PChar('USPlugin_Info'));
      if (@PInfo <> nil) then
      begin
        Info.cbSize := SizeOf(TUS_PluginInfo);

        try // Call info proc
          PInfo(@Info);
        except
          Info.Name := '';
          Core.ReportError(integer(PChar('Error getting plugin info: ' + Filename)), PChar('TPluginLoader'));
        end;

        // Is name set ?
        if (Trim(Info.Name) <> '') then
        begin
          PluginID := PluginExists(Info.Name);

          if (PluginID > 0) and (Plugins[PluginID].State >=4) then
            PluginID := -1;

          if (PluginID = -1) then
          begin
            // Add new item to array
            PluginID := Length(Plugins);
            SetLength(Plugins, PluginID + 1);

            // Fill with info:
            Plugins[PluginID].Info := Info;
            Plugins[PluginID].State := 0;
            Plugins[PluginID].Path := Filename;
            Plugins[PluginID].NeedsDeInit := false;
            Plugins[PluginID].hLib := hLib;

            // Try to get procs
            Plugins[PluginID].Procs.Load   := GetProcAddress (hLib, PChar('USPlugin_Load'));
            Plugins[PluginID].Procs.Init   := GetProcAddress (hLib, PChar('USPlugin_Init'));
            Plugins[PluginID].Procs.DeInit := GetProcAddress (hLib, PChar('USPlugin_DeInit'));

            if (@Plugins[PluginID].Procs.Load = nil) or (@Plugins[PluginID].Procs.Init = nil) or (@Plugins[PluginID].Procs.DeInit = nil) then
            begin
              Plugins[PluginID].State := 255;
              FreeLibrary(hLib);
              Core.ReportError(integer(PChar('Can''t get plugin procs from libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
            end;

            // Emulate loading process if this plugin is loaded too late
            if (LoadingProcessFinished) then
            begin
              CallLoad(PluginID);
              CallInit(PluginID);
            end;
          end
          else if (LoadingProcessFinished = false) then
          begin
            if (Plugins[PluginID].Info.Version < Info.Version) then
            begin // Found newer version of this plugin
              Core.ReportDebug(integer(PChar('Found a newer version of plugin: ' + string(Info.Name))), PChar('TPluginLoader'));

              // Unload old plugin
              UnloadPlugin(PluginID, nil);

              // Fill with new info
              Plugins[PluginID].Info := Info;
              Plugins[PluginID].State := 0;
              Plugins[PluginID].Path := Filename;
              Plugins[PluginID].NeedsDeInit := false;
              Plugins[PluginID].hLib := hLib;

              // Try to get procs
              Plugins[PluginID].Procs.Load   := GetProcAddress (hLib, PChar('USPlugin_Load'));
              Plugins[PluginID].Procs.Init   := GetProcAddress (hLib, PChar('USPlugin_Init'));
              Plugins[PluginID].Procs.DeInit := GetProcAddress (hLib, PChar('USPlugin_DeInit'));

              if (@Plugins[PluginID].Procs.Load = nil) or (@Plugins[PluginID].Procs.Init = nil) or (@Plugins[PluginID].Procs.DeInit = nil) then
              begin
                FreeLibrary(hLib);
                Plugins[PluginID].State := 255;
                Core.ReportError(integer(PChar('Can''t get plugin procs from libary: "' + Info.Name + '" ' + Filename)), PChar('TPluginLoader'));
              end;
            end
            else
            begin // Newer Version already loaded
              FreeLibrary(hLib);
            end;
          end
          else
          begin
            FreeLibrary(hLib);
            Core.ReportError(integer(PChar('Plugin with this name already exists: ' + string(Info.Name))), PChar('TPluginLoader'));
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
        Core.ReportError(integer(PChar('Can''t find info procedure: ' + Filename)), PChar('TPluginLoader'));
      end;
    end
    else
      Core.ReportError(integer(PChar('Can''t load plugin libary: ' + Filename)), PChar('TPluginLoader'));
  end;
end;

//--------------
// Calls load func of plugin with the given index
//--------------
function  TPluginLoader.CallLoad(Index: integer): integer;
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
      end;

      if (Result = 0) then
        Plugins[Index].State := 1
      else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(integer(PChar('Error calling load function from plugin: ' + string(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls init func of plugin with the given index
//--------------
function  TPluginLoader.CallInit(Index: integer): integer;
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
      end;

      if (Result = 0) then
      begin
        Plugins[Index].State := 2;
        Plugins[Index].NeedsDeInit := true;
      end
      else
      begin
        FreePlugin(Index);
        Plugins[Index].State := 255;
        Core.ReportError(integer(PChar('Error calling init function from plugin: ' + string(Plugins[Index].Info.Name))), PChar('TPluginLoader'));
      end;
    end;
  end;
end;

//--------------
// Calls deinit proc of plugin with the given index
//--------------
procedure TPluginLoader.CallDeInit(Index: integer);
begin
  if(Index < Length(Plugins)) then
  begin
    if (Plugins[Index].State < 4) then
    begin
      if (@Plugins[Index].Procs.DeInit <> nil) and (Plugins[Index].NeedsDeInit) then
        try
          Plugins[Index].Procs.DeInit(@PluginInterface);
        except

        end;

      // Don't forget to remove services and subscriptions by this plugin
      Core.Hooks.DelbyOwner(-1 - Index);

      FreePlugin(Index);
    end;
  end;
end;

//--------------
// Frees all plugin sources (procs and handles) - helper for deiniting functions
//--------------
procedure TPluginLoader.FreePlugin(Index: integer);
begin
  Plugins[Index].State := 4;
  Plugins[Index].Procs.Load := nil;
  Plugins[Index].Procs.Init := nil;
  Plugins[Index].Procs.DeInit := nil;

  if (Plugins[Index].hLib <> 0) then
    FreeLibrary(Plugins[Index].hLib);
end;

//--------------
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the plugin
//--------------
function TPluginLoader.LoadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: integer;
  sFile: string;
begin
  Result := -1;
  sFile := '';
  // lParam is ID
  if (lParam = nil) then
  begin
    Index := wParam;
  end
  else
  begin //lParam is PChar
    try
      sFile := string(PChar(lParam));
      Index := PluginExists(sFile);
      if (Index < 0) and FileExists(sFile) then
      begin // Is filename
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
// wParam PChar(PluginName/PluginPath) | wParam (if lParam = nil) ID of the plugin
//--------------
function TPluginLoader.UnloadPlugin(wParam: TwParam; lParam: TlParam): integer;
var
  Index: integer;
  sName: string;
begin
  Result := -1;
  // lParam is ID
  if (lParam = nil) then
  begin
    Index := wParam;
  end
  else
  begin // wParam is PChar
    try
      sName := string(PChar(lParam));
      Index := PluginExists(sName);
    except
      Index := -2;
    end;
  end;

  if (Index >= 0) and (Index < Length(Plugins)) then
      CallDeInit(Index)
end;

//--------------
// if wParam = -1 then (if lParam = nil then get length of moduleinfo array. if lparam <> nil then write array of TUS_PluginInfo to address at lparam) else (Get PluginInfo of plugin with Index(wParam) to address at lParam)
//--------------
function TPluginLoader.GetPluginInfo(wParam: TwParam; lParam: TlParam): integer;
var I: integer;
begin
  Result := 0;
  if (wParam > 0) then
  begin // Get info of 1 plugin
    if (lParam <> nil) and (wParam < Length(Plugins)) then
    begin
      try
        Result := 1;
        PUS_PluginInfo(lParam)^ := Plugins[wParam].Info;
      except

      end;
    end;
  end
  else if (lParam = nil) then
  begin // Get length of plugin (info) array
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
    end;
  end;

end;

//--------------
// if wParam = -1 then (if lParam = nil then get length of plugin state array. if lparam <> nil then write array of byte to address at lparam) else (return state of plugin with index(wParam))
//--------------
function TPluginLoader.GetPluginState(wParam: TwParam; lParam: TlParam): integer;
var I: integer;
begin
  Result := -1;
  if (wParam > 0) then
  begin // Get state of 1 plugin
    if (wParam < Length(Plugins)) then
    begin
      Result := Plugins[wParam].State;
    end;
  end
  else if (lParam = nil) then
  begin // Get length of plugin (info) array
    Result := Length(Plugins);
  end
  else // Write plugininfo array to address in lParam
  begin
    try
      for I := 0 to high(Plugins) do
        byte(Pointer(integer(lParam) + I)^) := Plugins[I].State;
      Result := Length(Plugins);
    except
      Core.ReportError(integer(PChar('Could not write pluginstate array')), PChar('TPluginLoader'));
    end;
  end;
end;

{*********************
  TtehPlugins
  Implementation
*********************}

//-------------
// function that gives some infos about the module to the core
//-------------
procedure TtehPlugins.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'TtehPlugins';
  pInfo^.Version := MakeVersion(1,0,0,chr(0));
  pInfo^.Description := 'Module executing the Plugins!';
end;

//-------------
// Just the constructor
//-------------
constructor TtehPlugins.Create;
begin
  inherited;
  PluginLoader := nil;
end;

//-------------
// Is called on loading.
// In this method only events and services should be created
// to offer them to other modules or plugins during the init process
// if false is returned this will cause a forced exit
//-------------
function TtehPlugins.Load: boolean;
var
  i: integer; // Counter
  CurExecutedBackup: integer; //backup of Core.CurExecuted Attribute
begin
  // Get pointer to pluginloader
  PluginLoader := PPluginLoader(Core.GetModulebyName('TPluginLoader'));
  if (PluginLoader = nil) then
  begin
    Result := false;
    Core.ReportError(integer(PChar('Could not get pointer to pluginLoader')), PChar('TtehPlugins'));
  end
  else
  begin
    Result := true;

    // Backup curexecuted
    CurExecutedBackup := Core.CurExecuted;

    // Start loading the plugins
    for i := 0 to High(PluginLoader.Plugins) do
    begin
      Core.CurExecuted := -1 - i;

      try
        // Unload plugin if not correctly executed
        if (PluginLoader.CallLoad(i) <> 0) then
        begin
          PluginLoader.CallDeInit(i);
          PluginLoader.Plugins[i].State := 254; // Plugin asks for unload
          Core.ReportDebug(integer(PChar('Plugin selfabort during loading process: ' + string(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
        end
        else
        begin
          Core.ReportDebug(integer(PChar('Plugin loaded succesfully: ' + string(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
        end;
      except
        // Plugin could not be loaded.
        // => Show error message, then shutdown plugin
        on E: Exception do
        begin
          PluginLoader.CallDeInit(i);
          PluginLoader.Plugins[i].State := 255; // Plugin causes error
          Core.ReportError(integer(PChar('Plugin causes error during loading process: ' + PluginLoader.Plugins[i].Info.Name + ', ErrorMsg: "' + E.Message + '"')), PChar('TtehPlugins'));
        end;
      end;
    end;

    // Reset CurExecuted
    Core.CurExecuted := CurExecutedBackup;
  end;
end;

//-------------
// Is called on init process
// in this method you can hook some events and create + init
// your classes, variables etc.
// if false is returned this will cause a forced exit
//-------------
function TtehPlugins.Init: boolean;
var
  i: integer; // Counter
  CurExecutedBackup: integer; // backup of Core.CurExecuted attribute
begin
  Result := true;

  // Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  // Start loading the plugins
  for i := 0 to High(PluginLoader.Plugins) do
    try
      Core.CurExecuted := -1 - i;

      // Unload plugin if not correctly executed
      if (PluginLoader.CallInit(i) <> 0) then
      begin
        PluginLoader.CallDeInit(i);
        PluginLoader.Plugins[i].State := 254; //Plugin asks for unload
        Core.ReportDebug(integer(PChar('Plugin selfabort during init process: ' + string(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
      end
      else
        Core.ReportDebug(integer(PChar('Plugin inited succesfully: ' + string(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
    except
      // Plugin could not be loaded.
      // => Show error message, then shut down plugin
      PluginLoader.CallDeInit(i);
      PluginLoader.Plugins[i].State := 255; //Plugin causes Error
      Core.ReportError(integer(PChar('Plugin causes error during init process: ' + string(PluginLoader.Plugins[i].Info.Name))), PChar('TtehPlugins'));
    end;

  // Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

//-------------
// Is called if this module has been inited and there is a exit.
// Deinit is in backwards initing order
//-------------
procedure TtehPlugins.DeInit;
var
  i: integer; // Counter
  CurExecutedBackup: integer; // backup of Core.CurExecuted attribute
begin
  // Backup CurExecuted
  CurExecutedBackup := Core.CurExecuted;

  // Start loop

  for i := 0 to High(PluginLoader.Plugins) do
  begin
    try
      // DeInit plugin
      PluginLoader.CallDeInit(i);
    except
    end;
  end;

  // Reset CurExecuted
  Core.CurExecuted := CurExecutedBackup;
end;

end.
