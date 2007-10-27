unit uPluginInterface;
{*********************
  uPluginInterface
  Unit fills a TPluginInterface Structur with Method Pointers
  Unit Contains all Functions called directly by Plugins
*********************}

interface
uses uPluginDefs;

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
  Function NotivyEventHooks (hEvent: THandle; wParam, lParam: dWord): integer; stdcall;

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
  Function CallService (ServiceName: PChar; wParam, lParam: dWord): integer; stdcall;

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
Function NotivyEventHooks (hEvent: THandle; wParam, lParam: dWord): integer; stdcall;
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
Function CallService (ServiceName: PChar; wParam, lParam: dWord): integer; stdcall;
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
