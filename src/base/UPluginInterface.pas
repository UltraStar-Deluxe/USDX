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

unit uPluginInterface;
{*********************
  uPluginInterface
  Unit fills a TPluginInterface structure with method pointers
  Unit contains all functions called directly by plugins
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  uPluginDefs;

//---------------
// Methods for Plugin
//---------------
  {******** Hook specific Methods ********}
    {Function Creates a new Hookable Event and Returns the Handle
     or 0 on Failure. (Name already exists)}
  Function CreateHookableEvent (EventName: PChar): THandle; stdcall;

    {Function Destroys an Event and Unhooks all Hooks to this Event.
     0 on success, not 0 on Failure}
  Function DestroyHookableEvent (hEvent: THandle): integer; stdcall;

    {Function start calling the Hook Chain
     0 if Chain is called until the End, -1 if Event Handle is not valid
     otherwise Return Value of the Hook that breaks the Chain}
  Function NotivyEventHooks (hEvent: THandle; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Hooks an Event by Name.
     Returns Hook Handle on Success, otherwise 0}
  Function HookEvent (EventName: PChar; HookProc: TUS_Hook): THandle; stdcall;

    {Function Removes the Hook from the Chain
     Returns 0 on Success}
  Function UnHookEvent (hHook: THandle): Integer; stdcall;

    {Function Returns Non Zero if a Event with the given Name Exists,
     otherwise 0}
  Function EventExists (EventName: PChar): Integer; stdcall;

    {******** Service specific Methods ********}
    {Function Creates a new Service and Returns the Services Handle
     or 0 on Failure. (Name already exists)}
  Function CreateService (ServiceName: PChar; ServiceProc: TUS_Service): THandle; stdcall;

    {Function Destroys a Service.
     0 on success, not 0 on Failure}
  Function DestroyService (hService: THandle): integer; stdcall;

    {Function Calls a Services Proc
     Returns Services Return Value or SERVICE_NOT_FOUND on Failure}
  Function CallService (ServiceName: PChar; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Returns Non Zero if a Service with the given Name Exists,
     otherwise 0}
  Function ServiceExists (ServiceName: PChar): Integer; stdcall;

implementation
uses UCore;

{******** Hook specific Methods ********}
//---------------
// Function Creates a new Hookable Event and Returns the Handle
// or 0 on Failure. (Name already exists) 
//---------------
Function CreateHookableEvent (EventName: PChar): THandle; stdcall;
begin
  Result := Core.Hooks.AddEvent(EventName);
end;

//---------------
// Function Destroys an Event and Unhooks all Hooks to this Event.
// 0 on success, not 0 on Failure
//---------------
Function DestroyHookableEvent (hEvent: THandle): integer; stdcall;
begin
  Result := Core.Hooks.DelEvent(hEvent);
end;

//---------------
// Function start calling the Hook Chain
// 0 if Chain is called until the End, -1 if Event Handle is not valid
// otherwise Return Value of the Hook that breaks the Chain
//---------------
Function NotivyEventHooks (hEvent: THandle; wParam: TwParam; lParam: TlParam): integer; stdcall;
begin
  Result := Core.Hooks.CallEventChain(hEvent, wParam, lParam);
end;

//---------------
// Function Hooks an Event by Name.
// Returns Hook Handle on Success, otherwise 0
//---------------
Function HookEvent (EventName: PChar; HookProc: TUS_Hook): THandle; stdcall;
begin
  Result := Core.Hooks.AddSubscriber(EventName, HookProc);
end;

//---------------
// Function Removes the Hook from the Chain
// Returns 0 on Success
//---------------
Function UnHookEvent (hHook: THandle): Integer; stdcall;
begin
  Result := Core.Hooks.DelSubscriber(hHook);
end;

//---------------
// Function Returns Non Zero if a Event with the given Name Exists,
// otherwise 0
//---------------
Function EventExists (EventName: PChar): Integer; stdcall;
begin
  Result := Core.Hooks.EventExists(EventName);
end;

    {******** Service specific Methods ********}
//---------------
// Function Creates a new Service and Returns the Services Handle
// or 0 on Failure. (Name already exists)
//---------------
Function CreateService (ServiceName: PChar; ServiceProc: TUS_Service): THandle; stdcall;
begin
  Result := Core.Services.AddService(ServiceName, ServiceProc);
end;

//---------------
// Function Destroys a Service.
// 0 on success, not 0 on Failure
//---------------
Function DestroyService (hService: THandle): integer; stdcall;
begin
  Result := Core.Services.DelService(hService);
end;
  
//---------------
// Function Calls a Services Proc
// Returns Services Return Value or SERVICE_NOT_FOUND on Failure
//---------------
Function CallService (ServiceName: PChar; wParam: TwParam; lParam: TlParam): integer; stdcall;
begin
  Result := Core.Services.CallService(ServiceName, wParam, lParam);
end;
   
//---------------
// Function Returns Non Zero if a Service with the given Name Exists,
// otherwise 0                                                      
//---------------
Function ServiceExists (ServiceName: PChar): Integer; stdcall;
begin
  Result := Core.Services.ServiceExists(ServiceName);
end;

end.
