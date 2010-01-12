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

unit UHookableEvent;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua;

type
  { Record holding information about a hook of an event }
  PHook = ^THook;
  THook = record
    Handle: Integer; //< Handle to identify the hook, e.g. for unhooking by plugin
    Parent: Integer; //< Lua Core Handle this hook belongs to

    Func: String;   //< Name of the global that holds the function

    Next: PHook; //< Next Hook in list (nil for the first)
  end;

  { procedure is called before each call to the hooking lua functions, to push values on stack
    returns the number of pushed arguments}
  PrepareStackProc = Function(L: PLua_State): Integer;

  { class representing a hookable event }
  THookableEvent = class
    private
      iHandle: Integer; //< used to unregister at lua core
      LastHook: PHook;  //< last hook in hook list, first to be called
      NextHookHandle: Integer;  //< handle to identify next hook

      sName: String;    //< the events name

      PrepareStack: PrepareStackProc; //< prepare stack procedure passed to constructor
      CallinProcess: boolean; //< true if a chain call is in process, to prepare unhooking during calls
      HooksToRemove: array of PHook; // hooks to delete after chaincall

      procedure RemoveWaitingHooks;
    public
      constructor Create(Name: String; const Proc: PrepareStackProc = nil);

      property Name: String read sName;             //< returns the events name
      property Handle: Integer read iHandle;        //< returns the events name

      procedure Hook(L: Plua_State; Parent: Integer; Func: String); //< pushes hook object/table to the lua stack
      procedure UnHook(L: Plua_State; hHook: Integer);              //< unhook by plugin. push true or error string to lua stack

      procedure UnHookByParent(Parent: Integer);     //< deletes all hooks by a specified parent (unhook by core)

      function CallHookChain(Breakable: Boolean): PLua_State;   //< calls the events hookchain. if breakable, plugin can breake the chain by returning a value != 0 or false or nil

      destructor Destroy; override;
  end;

{ the default function for THookableEvent.PrepareStack it don't pass any arguments }
function PrepareStack_Dummy(L: PLua_State): Integer;

{ function in resulting hook table. it calls the unhook command of the event on plugins demand }
function LuaHook_UnHook(L: Plua_State): Integer; cdecl;

implementation
uses ULuaCore;

constructor THookableEvent.Create(Name: String; const Proc: PrepareStackProc);
begin
  inherited Create;
  
  Self.sName := Name;

  if (@Proc = nil) then
    Self.PrepareStack := @PrepareStack_Dummy
  else
    Self.PrepareStack := Proc;

  //init LastHook pointer w/ nil
  LastHook := nil;
  NextHookHandle := 1;

  iHandle := LuaCore.RegisterEvent(Self);
end;

destructor THookableEvent.Destroy;
var
  Prev: PHook;
  Cur:  PHook;
begin
  //delete all hooks
  Cur := LastHook;
  While (Cur <> nil) do
  begin
    Prev := Cur;
    Cur := Prev.Next;

    Dispose(Prev);
  end;

  //remove from luacores list
  LuaCore.UnRegisterEvent(iHandle);

  inherited;
end;

{ adds hook to events list and pushes hook object/table to the lua stack }
procedure  THookableEvent.Hook(L: PLua_State; Parent: Integer; Func: String);
  var
    Item: PHook;
    P: TLuaPlugin;
begin
  P := LuaCore.GetPluginById(Parent);
  if (P <> nil) then
  begin
    // get mem and fill it w/ data
    New(Item);
    Item.Handle := NextHookHandle;
    Inc(NextHookHandle);

    Item.Parent := Parent;
    Item.Func := Func;

    // add at front of the hook chain
    Item.Next := LastHook;
    LastHook := Item;

    //we need 2 free stack slots
    lua_checkstack(L, 2);

    //create the hook table, we need 2 elements (event name and unhook function)
    lua_createtable(L, 0, 2);

    //push events name
    lua_pushstring(L, PAnsiChar(Name));

    //add the name to the table
    lua_setfield(L, -2, 'Event');

    //push hook id to the stack
    lua_pushinteger(L, Item.Handle);

    //create a c closure, append one value from stack(the id)
    //this will pop both, the function and the id
    lua_pushcclosure(L, LuaHook_UnHook, 1);

    //add the function to our table
    lua_setfield(L, -2, 'Unhook');

    //the table is left on the stack, it is our result
  end;
end;

{ removes hooks in HookstoRemove array from chain }
procedure THookableEvent.RemoveWaitingHooks;
  function IsInArray(Cur: PHook): boolean;
    var I: Integer;
  begin
    Result := false;
    for I := 0 to high(HooksToRemove) do
      if (HooksToRemove[I] = Cur) then
      begin
        Result := true;
        Break;
      end;
  end;

  var
    Cur, Prev: PHook;
begin
  Prev := nil;
  Cur := LastHook;

  while (Cur <> nil) do
  begin
    if (IsInArray(Cur)) then
    begin //we found the hook
      if (prev <> nil) then
        Prev.Next := Cur.Next
      else //last hook found
        LastHook := Cur.Next;

      //free hooks memory
      Dispose(Cur);

      if (prev <> nil) then
        Cur := Prev.Next
      else
        Cur := LastHook;
    end
    else
    begin
      Prev := Cur;
      Cur := Prev.Next;
    end;
  end;

  SetLength(HooksToRemove, 0);
end;

{ unhook by plugin. push true or error string to lua stack }
procedure  THookableEvent.UnHook(L: Plua_State; hHook: Integer);
  var
    Cur, Prev: PHook;
    Len: integer;
begin
  if (hHook < NextHookHandle) and (hHook > 0) then
  begin
    //Search for the Hook
    Prev := nil;
    Cur := LastHook;

    while (Cur <> nil) do
    begin
      if (Cur.Handle = hHook) then
      begin //we found the hook
        if not CallinProcess then
        begin // => remove it
          if (prev <> nil) then
            Prev.Next := Cur.Next
          else //last hook found
            LastHook := Cur.Next;

          //free hooks memory
          Dispose(Cur);
        end
        else
        begin // add to list of hooks to remove
          Len := Length(HooksToRemove);
          SetLength(HooksToRemove, Len + 1);
          HooksToRemove[Len] := Cur;
        end;
        
        //indicate success
        lua_pushboolean(L, True);
        exit; //break the chain and exit the function
      end;
      Prev := Cur;
      Cur := Prev.Next;
    end;

    lua_pushstring(L, PAnsiChar('handle already unhooked')); //the error description
  end
  else
    lua_pushstring(L, PAnsiChar('undefined hook handle')); //the error description
end;

{ deletes all hooks by a specified parent (unhook by core) }
procedure  THookableEvent.UnHookByParent(Parent: Integer);
  var
    Cur, Prev: PHook;
begin
  Prev := nil;
  Cur := LastHook;

  While (Cur <> nil) do
  begin
    if (Cur.Parent = Parent) then
    begin //found a hook from parent => remove it
      if (Prev <> nil) then
        Prev.Next := Cur.Next
      Else
        LastHook := Cur.Next;

      Dispose(Cur);

      if (Prev <> nil) then
        Cur := Prev.Next
      else
        Cur := LastHook;
    end
    else //move through the chain
    begin
      Prev := Cur;
      Cur := Prev.Next;
    end;
  end;
end;

{ calls the events hookchain. if breakable, plugin can breake the chain
  by returning a value
  breakable is pushed as the first parameter to the hooking functions
  if chain is broken the LuaStack is returned, with all results left
  you may call lua_clearstack }
function  THookableEvent.CallHookChain(Breakable: Boolean): Plua_State;
  var
    Cur: PHook;
    P: TLuaPlugin;
begin
  Result := nil;

  CallinProcess := true;
  
  Cur := LastHook;
  While (Cur <> nil) do
  begin
    P := LuaCore.GetPluginById(Cur.Parent);
    lua_pushboolean(P.LuaState, Breakable);

    if  (P.CallFunctionByName(Cur.Func, 1 + PrepareStack(P.LuaState), LUA_MULTRET))
    and Breakable
    and (lua_gettop(P.LuaState) > 0) then
    begin //Chain Broken
      Result := P.LuaState;
      Break;
    end;

    Cur := Cur.Next;
  end;

  RemoveWaitingHooks;
  CallinProcess := false;
end;

{ the default function for THookableEvent.PrepareStack it don't pass any arguments }
function PrepareStack_Dummy(L: PLua_State): Integer;
begin
  Result := 0;
end;

{ function in resulting hook table. it calls the unhook command of the event on plugins demand }
function LuaHook_UnHook(L: Plua_State): Integer; cdecl;
  var
    Name: string;
    Event: THookableEvent;
    hHook: integer;
begin
  Result := 0;

  if not lua_isTable(L, 1) then
    LuaL_Error(L, 'Can''t find hook table in LuaHook_Unhook. Please call Unhook with method seperator (colon) instead of a point.');

  // get event name
  Lua_GetField(L, 1, 'Event');
  if not lua_isString(L, -1) then
    LuaL_Error(L, 'Can''t get event name in LuaHook_Unhook');

  Name := Lua_ToString(L, -1);

  // get event by name
  Event := LuaCore.GetEventbyName(Name);

  // free stack slots
  Lua_pop(L, Lua_GetTop(L));

  if (Event = nil) then
    LuaL_Error(L, PAnsiChar('event ' + Name + ' does not exist (anymore?) in LuaHook_Unhook'));

  // get the hookid
  hHook := lua_ToInteger(L, lua_upvalueindex(1));

  Event.UnHook(L, hHook);
end;

end.