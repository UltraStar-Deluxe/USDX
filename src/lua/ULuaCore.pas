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

unit ULuaCore;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  UHookableEvent,
  ULua,
  UPath;

type
  { this exception is raised when the lua panic function
    is called. Only in case we use call instead of pcall.
    it has the lua error string in its message attribute }
  ELuaException = class(Exception);

  { record represents item of Eventlist of TLuaCore }
  PEventListItem = ^TEventListItem;
  TEventListItem = record
    Event: THookableEvent;
    Next:  PEventListItem;
  end;

  { record represents a module }
  TLuaModule = record
    Name:      string;
    Functions: array of luaL_reg; // modules functions, with trailing nils this time
  end;

  TLuaPlugin_Status = (psNone, psRunning, psClosed, psErrorOnLoad, psErrorOnCall, psErrorInInit, psErrorOnRun);
  { class represents a loaded plugin }
  TLuaPlugin = class
    private
      iId:        integer;
      Filename:   IPath;
      State:      Plua_State; //< all functions of this plugin are called with this Lua state
      bPaused:    boolean;    //< If true no lua functions from this state are called
      ErrorCount: integer;    //< counts the errors that occured during function calls of this plugin
      ShutDown:   boolean;    //< for self shutdown by plugin. true if plugin wants to be unloaded after execution of current function

      sName:    string;
      sVersion: string;
      sAuthor:  string;
      sURL:     string;

      sStatus:  TLuaPlugin_Status;
    public
      constructor Create(Filename: IPath; Id: integer);

      property Id:      integer read iId;
      property Name:    string read sName;
      property Version: string read sVersion;
      property Author:  string read sAuthor;
      property Url:     string read sUrl;

      property Status:      TLuaPlugin_Status read sStatus;
      property CountErrors: integer read ErrorCount;

      property LuaState:    Plua_State read State;

      procedure Load;

      procedure Register(Name, Version, Author, Url: string);
      function  HasRegistered: boolean;

      procedure PausePlugin(doPause: boolean);
      property Paused: boolean read bPaused write PausePlugin;

      procedure ShutMeDown;

      { calls the lua function in the global with the given name.
        the arguments to the function have to be pushed to the stack
        before calling this function.
        the arguments and the function will be removed from stack
        results will not be removed.
        if result is false there was an error calling the function
        if ReportErrors is true the errorstring is popped from stack
        and written to error.log otherwise it is left on stack}
      function CallFunctionByName(Name:               string; 
                                  const nArgs:        integer = 0;
				  const nResults:     integer = 0;
				  const ReportErrors: boolean = true): boolean;
      procedure ClearStack;

      procedure Unload; //< Destroys the Luastate, and frees as much mem as possible, without destroying the class and important information 

      destructor Destroy; override;
  end;

  { class managing the plugins with their LuaStates, the events and modules
    it also offers the usdx table to the plugins with some basic functionality
    like self unload or hook getting}
  TLuaCore = class
    private
      EventList:         PEventListItem;  //< pointer to first registred Event, ordered by name
      EventHandles:      array of string; //< Index is Events handle, value is events name. if length(value) is 0 handle is considered unregistred

      Plugins:           array of TLuaPlugin;

      eLoadingFinished: THookableEvent;
    protected
      Modules: array of TLuaModule; //< modules that has been registred, has to be proctected because fucntions of this unit need to get access

      function GetModuleIdByName(Name: string): integer; // returns id of given module, or -1 if module is not found
    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadPlugins;                         //< calls LoadPlugin with Plugindir and LoadingFinished Eventchain

      procedure BrowseDir(Dir: IPath);               //< searches for files with extension .usdx in the specified dir and tries to load them with lua
      procedure LoadPlugin(Filename: IPath);         //< tries to load filename with lua and creates the default usdx lua environment for the plugins state

      function GetPluginByName(Name: string): TLuaPlugin;
      function GetPluginById(Id: integer): TLuaPlugin;

      { this function adds a module loader for your functions
        name is the name the script needs to write in its require()
        Functions is an array of lua calling compatible functions
        without trailing nils! }
      procedure RegisterModule(Name: string; const Functions: array of luaL_reg);

      function RegisterEvent(Event: THookableEvent): integer; //< adds the event to eventlist and returns its handle
      procedure UnRegisterEvent(hEvent: integer);             //< removes the event from eventlist by handle

      function GetEventbyName(Name: string): THookableEvent;       //< tries to find the event with the given name in the list
      function GetEventbyHandle(hEvent: integer): THookableEvent;  //< tries to find the event with the given handle

      procedure UnHookByParent(Parent: integer); //< remove all hooks by given parent id from all events

      procedure PrepareState(L: Plua_State);

      procedure DumpPlugins; //< prints plugin runtime information with Log.LogStatus
  end;

// some luastyle functions to call from lua scripts
{ register global, used by plugins to identify
  register(plugin name, plugin version, [plugin author], [plugin homepage])
  can only be called once since the global "register" is niled by the function
  returns true on success. (name does not exist)}
function TLuaPlugin_Register (L: Plua_State): integer; cdecl;

{ moduleloader for usdx.* modules
  stored in package.loaders[3]
  package.loaders[3] (module name)
  returns a function to load the requested module or a error
  description(string) when the module is not found }
function TLuaCore_ModuleLoader (L: Plua_State): integer; cdecl;

{ loads module specified by a cfunction upvalue to
  usdx.modulename and returns it.
  loadmodule(module name) }
function TLuaCore_LoadModule (L: Plua_State): integer; cdecl;

{ custom lua panic function
  it writes error string to error.log and raises an ELuaException
  that may be caught }
function TLua_CustomPanic (L: Plua_State): integer; cdecl;

{ replacement for luas require function
  can be called with more than one parameter to require
  some modules at once. e.g.: require('math', 'Usdx.Log')
  modules are loaded from right to left
  unlike standard require the module tables are not returned
  the standard require function in _require is called by
  this function }
function TLua_CustomRequire(L: PLua_State): integer; cdecl;


var
  LuaCore: TLuaCore;

implementation
uses
  StrUtils,
  ULog,
  UFilesystem,
  ULuaUsdx,
  UPathUtils,
  ULuaUtils;

constructor TLuaCore.Create;
begin
  inherited;

  // init EventList with nil
  EventList := nil;

  eLoadingFinished := nil;
end;

destructor TLuaCore.Destroy;
var
  Cur:  PEventListItem;
  Prev: PEventListItem;
begin
  SetLength(EventHandles, 0);

  // delete event list
  Cur := EventList;

  while(Cur <> nil) do
  begin
    Prev := Cur;
    Cur := Prev.Next;

    Dispose(Prev);
  end;

  inherited;
end;

{ calls BrowseDir with plugin dir and LoadingFinished eventchain }
procedure TLuaCore.LoadPlugins;
begin
  // we have to create event here, because in create it can
  // not be registred, because LuaCore is no assigned
  if (not Assigned(eLoadingFinished)) then
    eLoadingFinished := THookableEvent.Create('Usdx.LoadingFinished');

  BrowseDir(PluginPath);
  eLoadingFinished.CallHookChain(false);
end;

{ searches for files with extension .usdx in the specified
  dir and tries to load them with lua }
procedure TLuaCore.BrowseDir(Dir: IPath);
  var
    Iter:     IFileIterator;
    FileInfo: TFileInfo;
    FileName: IPath;
    Ext:      IPath;
begin
  Ext := Path('.usdx');

  // search for all files and directories
  Iter := FileSystem.FileFind(Dir.Append('*'), faAnyFile);
  while (Iter.HasNext) do
  begin
    FileInfo := Iter.Next;
    FileName := FileInfo.Name;
    if ((FileInfo.Attr and faDirectory) <> 0) then
    begin
      if (not FileName.Equals('.')) and (not FileName.Equals('..')) then
        BrowseDir(Dir.Append(FileName));
    end
    else
    begin
      if (Ext.Equals(FileName.GetExtension(), true)) then
      begin
        LoadPlugin(Dir.Append(FileName));
      end;
    end;
  end;
end;

{ tries to load filename with lua and creates the default
  usdx lua environment for the plugins state }
procedure TLuaCore.LoadPlugin(Filename: IPath);
  var
    Len: integer;  
begin
  Len := Length(Plugins);
  SetLength(Plugins, Len + 1);
  Plugins[Len] := TLuaPlugin.Create(Filename, Len);
  Plugins[Len].Load;
end;

{ returns Plugin on success nil on failure }
function TLuaCore.GetPluginByName(Name: string): TLuaPlugin;
  var
    I: integer;
begin
  Result := nil;
  Name := lowercase(Name);

  for I := 0 to High(Plugins) do
    if (lowercase(Plugins[I].Name) = Name) then
    begin
      Result := GetPluginById(I);
      Exit;
    end;
end;

{ returns Plugin on success nil on failure }
function TLuaCore.GetPluginById(Id: integer): TLuaPlugin;
begin
  if (Id >= 0) and (Id <= High(Plugins)) then
    Result := Plugins[Id]
  else
    Result := nil;
end;

{ this function adds a module loader for your functions
  name is the name the script needs to write in its require()
  Functions is an array of lua calling compatible functions
  without trailing nils! }
procedure TLuaCore.RegisterModule(Name: string; const Functions: array of luaL_reg);
  var
    Len:     integer;
    FuncLen: integer;
    I:       integer;
begin
  Len := Length(Modules);
  SetLength(Modules, Len + 1);
  Modules[Len].Name := Name;

  FuncLen := Length(Functions);
  SetLength(Modules[Len].Functions, FuncLen + 1);
  
  for I := 0 to FuncLen-1 do
    Modules[Len].Functions[I] := Functions[I];

  Modules[Len].Functions[FuncLen].name := nil;
  Modules[Len].Functions[FuncLen].func := nil;
end;

{ adds the event to eventlist and returns its handle
  called by THookableEvent on creation }
function TLuaCore.RegisterEvent(Event: THookableEvent): integer;
var
  Cur, Prev, Item: PEventListItem;
begin
  if (Event <> nil) and (Length(Event.Name) > 0) then
  begin
    Result := Length(EventHandles);
    SetLength(EventHandles, Result + 1); // get Handle and copy it to result

    EventHandles[Result] := Event.Name;

    // create eventlist item
    New(Item);
    Item.Event := Event;

    // search for a place for this event in alphabetical order
    Prev := nil;
    Cur := EventList;

    while (Cur <> nil) and (CompareStr(Cur.Event.Name, EventHandles[Result]) < 0) do
    begin
      Prev := Cur;
      Cur := Prev.Next;
    end;

    // found the place => add new item
    if (Prev <> nil) then
      Prev.Next := Item
    else // first item
      EventList := Item;

    Item.Next := Cur;
  end
  else
    Result := -1;
end;

{ removes the event from eventlist by handle }
procedure TLuaCore.UnRegisterEvent(hEvent: integer);
  var
    Cur, Prev: PEventListItem;
begin
  if (hEvent >= 0) and (hEvent <= High(EventHandles)) and (Length(EventHandles[hEvent]) > 0) then
  begin // hEvent in bounds and not already deleted
    // delete from eventlist
    Prev := nil;
    Cur  := EventList;

    while (Cur <> nil) and (CompareStr(Cur.Event.Name, EventHandles[hEvent]) < 0) do
    begin
      Prev := Cur;
      Cur  := Prev.Next;
    end;

    if (Cur <> nil) and (Cur.Event.Name = EventHandles[hEvent]) then
    begin // delete if found
      Prev.Next := Cur.Next; // remove from list
      Dispose(Cur); // free memory
    end;

    // delete from handle array
    EventHandles[hEvent] := '';
  end;
end;

{ tries to find the event with the given name in the list
  to-do : use binary search algorithm instead of linear search here
          check whether this is possible (events are saved in a pointer list) }
function TLuaCore.GetEventbyName(Name: string): THookableEvent;
  var
    Cur: PEventListItem;
begin
  Result := nil;

  if (Length(Name) > 0) then
  begin
    // search in eventlist
    Cur := EventList;

    while (Cur <> nil) and (CompareStr(Cur.Event.Name, Name) < 0) do
    begin
      Cur := Cur.Next;
    end;

    if (Cur <> nil) and (Cur.Event.Name = Name) then
    begin // we found what we want to find
      Result := Cur.Event;
    end;
  end;
end;

{ tries to find the event with the given handle }
function TLuaCore.GetEventbyHandle(hEvent: integer): THookableEvent;
begin
  if (hEvent >= 0) and (hEvent <= High(EventHandles)) and (Length(EventHandles[hEvent]) > 0) then
  begin // hEvent in bounds and not already deleted
    Result := GetEventByName(EventHandles[hEvent]);
  end
  else
    Result := nil;
end;

{ remove all hooks by given parent id from all events }
procedure TLuaCore.UnHookByParent(Parent: integer);
  var
    Cur: PEventListItem;
begin
  if (Parent >= 0) and (Parent <= High(Plugins)) then
  begin
    // go through event list
    Cur := EventList;

    while (Cur <> nil) do
    begin
      Cur.Event.UnHookByParent(Parent);
      Cur := Cur.Next;
    end;
  end;
end;

{ prepares the given already opened Lua state with the
  basic usdx environment, e.g.: base and package Modules,
  usdx moduleloader and usdx table }
procedure TLuaCore.PrepareState(L: Plua_State);
begin
  // load basic lib functionality
  lual_openLibs(L);
  lua_pop(L,1);

  // get package.searchers (former package.loaders) table
  lua_getglobal (L, PChar('package'));
{$IF LUA_VERSION_NUM >= 502}
  lua_getfield(L,-1,PChar('searchers'));
{$ELSE}
  lua_getfield(L,-1,PChar('loaders'));
{$IFEND}

  {**** Move C-Library and all-in-one module loader backwards,
        slot 3 is free now }
  // get package.loaders[4] function
  lua_pushinteger(L, 5); // push new index
  lua_pushinteger(L, 4); // push old index
  lua_gettable (L, -3);

  // and move it to package.loaders[5]
  lua_settable (L, -3);

  // get package.loaders[3] function
  lua_pushinteger(L, 4); // push new index
  lua_pushinteger(L, 3); // push old index
  lua_gettable (L, -3);

  // and move it to package.loaders[4]
  lua_settable (L, -3);

  {**** now we add the core module to package.loaders[3] }
  lua_pushinteger(L, 3); // push new loaders index
  lua_pushcfunction(L, TLuaCore_ModuleLoader);

  // and move it to package.loaders[3]
  lua_settable (L, -3);

  // pop both package and package.loaders tables from stack
  lua_pop(L, 2);

  {**** replace the standard require with our custom require function }
  // first move standard require function to _require
  lua_getglobal(L, PChar('require'));

  lua_setglobal(L, PChar('_require'));

  // then save custom require function to require
  lua_pushcfunction(L, TLua_CustomRequire);

  lua_setglobal(L, PChar('require'));

  {**** now we create the usdx table }
  // at first functions from ULuaUsdx
  luaL_register(L, 'Usdx', @ULuaUsdx_Lib_f[0]);
end;

{ returns id of given module, or -1 if module is not found }
function TLuaCore.GetModuleIdByName(Name: string): integer;
  var
    I: integer;
begin
  Result := -1;
  
  for I := 0 to High(Modules) do
    if (Modules[I].Name = Name) then
    begin
      Result := I;
      Exit;
    end;
end;

{ moduleloader for usdx.* modules
  stored in package.loaders[3]
  package.loaders[3] (module name)
  returns a function to load the requested module or an error
  description(string) when the module is not found }
function TLuaCore_ModuleLoader (L: Plua_State): integer; cdecl;
  var
    Name: string;
    ID:   integer;
begin
  Result := 1; // we will return one value in any case (or never return in case of an error)

  if (lua_gettop(L) >= 1) then
  begin
    // pop all arguments but the first
    if (lua_gettop(L) > 1) then
      lua_pop(L, lua_gettop(L)-1);


    if (lua_IsString(L, 1)) then
    begin // we got the name => go get it
      Name := lua_toString(L, 1);

      // we need at least 6 letters
      // and first 5 letters have to be usdx.
      if (Length(Name) > 5) and (lowercase(copy(Name, 1, 5))='usdx.') then
      begin
        ID := LuaCore.GetModuleIdByName(copy(Name, 6, Length(Name) - 5));
        if (ID >= 0) then
        begin // found the module -> return loader function
          lua_pushinteger(L, Id);
          lua_pushcclosure(L, TLuaCore_LoadModule, 1);
          // the function is the result, so we leave it on stack
        end
        else
          lua_pushString(L, PChar('usdx module "' + Name + '" couldn''t be found'));
      end
      else
        lua_pushString(L, PChar('module doesn''t have "Usdx." prefix'));

    end
    else
      luaL_argerror(L, 1, PChar('string expected'));
  end
  else
    luaL_error(L, PChar('no modulename specified in usdx moduleloader')); 
end;

{ loads module specified by a cfunction upvalue to
  usdx.modulename and returns it.
  loadmodule(module name) }
function TLuaCore_LoadModule (L: Plua_State): integer; cdecl;
  var
    Id: integer;
begin
  if (not lua_isnoneornil(L, lua_upvalueindex(1))) then
  begin
    Id := lua_ToInteger(L, lua_upvalueindex(1));
{$IF LUA_VERSION_NUM >= 502}
    // set module table as a field of the global Usdx
    lua_getglobal(L,Pchar('Usdx'));
    luaL_register(L, PChar(LuaCore.Modules[Id].Name), @LuaCore.Modules[Id].Functions[0]);
    lua_setfield(L, -2, PChar(LuaCore.Modules[Id].Name));
{$ELSE}
    luaL_register(L, PChar('Usdx.' + LuaCore.Modules[Id].Name), @LuaCore.Modules[Id].Functions[0]);

    // set the modules table as global "modulename"
    // so it can be accessed either by Usdx.modulename.x() or
    // by modulename.x()
    lua_setglobal(L, PChar(LuaCore.Modules[Id].Name));

    // no we net to push the table again to return it
    lua_getglobal(L, PChar(LuaCore.Modules[Id].Name));
{$IFEND}
    Result := 1; // return table
  end
  else
    luaL_error(L, PChar('no upvalue found in LuaCore_LoadModule'));
end;

{ prints plugin runtime information with Log.LogStatus }
procedure TLuaCore.DumpPlugins;
  function PluginStatusToString(Status: TLuaPlugin_Status): string;
  begin
    case Status of
      psNone:        Result := 'not loaded';
      psRunning:     Result := 'running';
      psClosed:      Result := 'closed';
      psErrorOnLoad: Result := 'error during load';
      psErrorOnCall: Result := 'error during call';
      psErrorInInit: Result := 'error in plugin_init()';
      psErrorOnRun:  Result := 'error on function call';
      else           Result := 'unknown';
    end;
  end;

var
  I: integer;
begin
  // print table header
  Log.LogStatus(Format('%3s %-30s %-8s %-10s %-7s %-6s', [
      '#', 'Name', 'Version', 'Status', 'Paused', '#Errors'
  ]), 'LuaCore Plugins');

  for I := 0 to High(Plugins) do
    Log.LogStatus(Format('%3d %-30s %-8s %-10s %-7s %-6d', [
        Plugins[I].Id, Plugins[I].Name, Plugins[I].Version,
        PluginStatusToString(Plugins[I].Status),
        BoolToStr(Plugins[I].Paused, true),
        Plugins[I].CountErrors
    ]), 'LuaCore Plugins');
  if (High(Plugins) < 0) then
    Log.LogError(' no plugins loaded ', 'LuaCore Plugins');
end;

// Implementation of TLuaPlugin
//--------
constructor TLuaPlugin.Create(Filename: IPath; Id: integer);
begin
  inherited Create;
  Self.iId := Id;
  Self.Filename := Filename;

  // set some default attributes
  Self.bPaused    := false;
  Self.ErrorCount := 0;
  Self.sName      := 'not registred';
  Self.sStatus    := psNone;
  Self.ShutDown   := false;

  State := nil; //< to prevent calls to unopened state
end;

destructor TLuaPlugin.Destroy;
begin
  Unload;
  inherited;
end;

{ does the main loading part
  can not be called by create, because Plugins[Id] isn't defined there }
procedure TLuaPlugin.Load;
begin
  // create Lua state for this plugin
  State := luaL_newstate;

  // set our custom panic function if s/t went wrong along the init
  // we don't expect
  lua_atPanic(State, TLua_CustomPanic);

  if (LuaL_LoadFile(State, PChar(Filename.ToNative)) = 0) then
  begin // file loaded successful
    { note: we run the file here, but the environment isn't
            set up now. it just causes the functions to
            register in globals and runs the code in the file
            body. At least there should be no code, it could
            neither use functions from baselibs nor load libs
            with require, this code would be useless. }
    if (lua_pcall(State, 0, 0, 0) = 0) then
    begin // file called successful

      // let the core prepare our state
      LuaCore.PrepareState(State);

      // set register function
      lua_checkstack(State, 2);
      lua_pushinteger(State, Id);
      lua_pushcclosure(State, TLuaPlugin_Register, 1); 
      lua_setglobal(State, PChar('register'));

      // write plugin id to registry
      lua_pushinteger(State, iId);
      lua_setfield (State, LUA_REGISTRYINDEX, '_USDX_STATE_ID');
      lua_pop(State, Lua_GetTop(State));

      // now run the plugin_init function
      // plugin_init() if false or nothing is returned plugin init is aborted
      if (CallFunctionByName('plugin_init', 0, 1)) then
      begin
        if (HasRegistered) and (sStatus = psNone) and (lua_toBoolean(State, 1)) then
        begin
          sStatus := psRunning;
          ClearStack;
        end
        else
          Unload;
      end
      else
      begin
        sStatus := psErrorInInit;
        Log.LogError('error in plugin_init: ' + Self.Filename.ToNative, 'lua');
        Unload;
      end;
    end
    else
    begin
      sStatus := psErrorOnLoad;
      Log.LogError(String(lua_toString(State, 1)), 'lua');
      Log.LogError('unable to call file: ' + Self.Filename.ToNative, 'lua');
      Unload;
    end;

  end
  else
  begin
    sStatus := psErrorOnLoad;
    Log.LogError(String(lua_toString(State, 1)), 'lua');
    Log.LogError('unable to load file: ' + Self.Filename.ToNative, 'lua');
    Unload;
  end;
end;

procedure TLuaPlugin.Register(Name, Version, Author, Url: string);
begin
  sName    := Name;
  sVersion := Version;
  sAuthor  := Author;
  sURL     := Url;
end;

{ returns true if plugin has called register }
function TLuaPlugin.HasRegistered: boolean;
begin
  Result := (Self.sName <> 'not registred');
end;

procedure TLuaPlugin.PausePlugin(doPause: boolean);
begin
  bPaused := doPause;
end;

{ unload plugin after execution of the current function }
procedure TLuaPlugin.ShutMeDown;
begin
  ShutDown := true;
end;

{ calls the lua function in the global with the given name.
  the arguments to the function have to be pushed to the stack
  before calling this function.
  the arguments and the function will be removed from stack
  results will not be removed.
  if result is false there was an error calling the function,
  if ReportErrors is true the errorstring is popped from stack
  and written to error.log otherwise it is left on stack}
function TLuaPlugin.CallFunctionByName(Name:               string; 
                                       const nArgs:        integer;
				       const nResults:     integer;
				       const ReportErrors: boolean): boolean;
begin
  Result := false;
  if (State <> nil) then
  begin
    if (not bPaused) then
    begin
      // we need at least one stack slot free
      lua_checkstack(State, 1);

      lua_getglobal(State, PChar(Name));

      if (lua_isfunction(State, -1)) then
      begin // we got a function
        // move function in front of the arguments (if any)
        if (nArgs > 0) then
          lua_insert(State, -(nArgs + 1));

        // call it!
        if (lua_pcall(State, nArgs, nResults, 0) = 0) then
          Result := true // called without errors
        else // increase error counter
          Inc (ErrorCount);
      end
      else
      begin // we have to pop the args and the field we pushed from stack
        lua_pop(State, nArgs + 1);
        // leave an errormessage on stack
        lua_pushstring(State, Pchar('could not find function named ' + Name));
      end;
    end
    else
    begin // we have to pop the args from stack
      lua_pop(State, nArgs);
      // leave an errormessage on stack
      lua_pushstring(State, PChar('plugin paused'));
    end;

    if (not Result) and (ReportErrors) then
      Log.LogError(lua_toString(State, -1), 'lua/' + sName);

    if ShutDown then
    begin // plugin indicates self shutdown
      ShutDown := false;
      Unload;
      Result := false;
    end
  end
  else
  begin
    Log.LogError('trying to call function of closed or not opened lua state', IfThen(HasRegistered, Name, Filename.ToUTF8));
  end;
end;

{ removes all values from stack }
procedure TLuaPlugin.ClearStack;
begin
  if (State <> nil) and (lua_gettop(State) > 0) then
    lua_pop(State, lua_gettop(State));
end;

{ destroys the lua state, and frees as much mem as possible,
  without destroying the class and important information }
procedure TLuaPlugin.Unload;
begin
  if (State <> nil) then
  begin
    if (Status in [psRunning, psErrorOnRun]) then
      CallFunctionByName('plugin_unload');

    ClearStack;
    lua_close(State);
    State := nil; // don't forget to nil it ;)

    LuaCore.UnHookByParent(iId);

    if (sStatus = psRunning) then
      sStatus := psClosed;
  end;
end;

function TLuaPlugin_Register (L: Plua_State): integer; cdecl;
  var
    Id: integer;
    P:  TLuaPlugin;
    Name, Version, Author, Url: string;
begin
  if (lua_gettop(L) >= 2) then
  begin // we got at least name and version
    if (not lua_isNumber(L, lua_upvalueindex(1))) then
      luaL_Error(L, PChar('upvalue missing'));

    if (not lua_isString(L, 1)) then
      luaL_ArgError(L, 1, 'string expected');

    if (not lua_isString(L, 2)) then
      luaL_ArgError(L, 1, 'string expected');

    Id := lua_ToInteger(L, lua_upvalueindex(1));

    // get version and name
    Name    := lua_tostring(L, 1);
    Version := lua_tostring(L, 2);

    // get optional parameters
    if (lua_isString(L, 3)) then // author
      Author := lua_toString(L, 3)
    else
    begin
      Author := 'unknown';
    end;

    // homepage
    if (lua_isString(L, 4)) then
      Url := lua_toString(L, 4)
    else
    begin
      Url := '';
    end;

    // clear stack
    if (lua_gettop(L) > 0) then
      lua_pop(L, lua_gettop(L));

    // call register
    P := LuaCore.GetPluginById(Id);
    if (P <> nil) then
      P.Register(Name, Version, Author, Url)
    else
      luaL_error(L, PChar('wrong id in upstream'));

    // remove function from global register
    lua_pushnil(L);
    lua_setglobal(L, PChar('register'));

    // return true
    Result := 1;
    lua_pushboolean(L, true);  
  end
  else
    luaL_error(L, PChar('not enough arguments, at least 2 expected. in TLuaPlugin_Register'));
end;

{ custom lua panic function
  it writes error string to error.log and raises an ELuaException
  that may be caught }
function TLua_CustomPanic (L: Plua_State): integer; cdecl;
  var
    Msg: string;
begin
  if (lua_isString(L, -1)) then
    Msg := lua_toString(L, -1)
  else
    Msg := 'undefined lua panic';

  Log.LogError(Msg, 'lua');

  raise ELuaException.Create(Msg);;

  Result := 0;
end;

{ replacement for luas require function
  can be called with more than one parameter to require
  some modules at once. e.g.: require('math', 'Usdx.Log')
  modules are loaded from right to left
  unlike standard require the module tables are not returned
  the standard require function in _require is called by
  this function }
function TLua_CustomRequire(L: PLua_State): integer; cdecl;
begin
  // no results
  Result := 0;

  // move through parameters
  while (lua_getTop(L) >= 1) do
  begin
    lua_checkstack(L,1);
    // get luas require function
    lua_getglobal(L, PChar('_require'));

    // move it under the top param
    lua_insert(L, -2);

    // call it with next param (function + param are poped from stack)
    lua_call(L, 1, 0);
  end;
end;

end.
