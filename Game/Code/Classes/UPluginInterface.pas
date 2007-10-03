unit uPluginInterface;
{*********************
  uPluginInterface
  Unit fills a TPluginInterface Structur with Method Pointers
  Unit Contains all Functions called directly by Plugins
*********************}

interface
uses uPluginDefs;

//---------------
// Procedure that Sets the PluginInterface Record
//---------------
  Procedure Init_PluginInterface;

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

var
  PluginInterface: TUS_PluginInterface;

implementation

//---------------
// Procedure that Sets the PluginInterface Record
//---------------
Procedure Init_PluginInterface;
begin
  PluginInterface.CreateHookableEvent := CreateHookableEvent;
  PluginInterface.DestroyHookableEvent := DestroyHookableEvent;
  PluginInterface.NotivyEventHooks := NotivyEventHooks;
  PluginInterface.HookEvent := HookEvent;
  PluginInterface.UnHookEvent := UnHookEvent;
  PluginInterface.EventExists := EventExists;

  PluginInterface.CreateService := CreateService;
  PluginInterface.DestroyService := DestroyService;
  PluginInterface.CallService := CallService;
  PluginInterface.ServiceExists := ServiceExists;
end;


{******** Hook specific Methods ********}
//---------------
// Function Creates a new Hookable Event and Returns the Handle
// or 0 on Failure. (Name already exists) 
//---------------
Function CreateHookableEvent (EventName: PChar): THandle; stdcall;
begin

end;

//---------------
// Function Destroys an Event and Unhooks all Hooks to this Event.
// 0 on success, not 0 on Failure
//---------------
Function DestroyHookableEvent (hEvent: THandle): integer; stdcall;
begin

end;

//---------------
// Function start calling the Hook Chain
// 0 if Chain is called until the End, -1 if Event Handle is not valid
// otherwise Return Value of the Hook that breaks the Chain
//---------------
Function NotivyEventHooks (hEvent: THandle; wParam, lParam: dWord): integer; stdcall;
begin

end;

//---------------
// Function Hooks an Event by Name.
// Returns Hook Handle on Success, otherwise 0
//---------------
Function HookEvent (EventName: PChar; HookProc: TUS_Hook): THandle; stdcall;
begin

end;

//---------------
// Function Removes the Hook from the Chain
// Returns 0 on Success
//---------------
Function UnHookEvent (hHook: THandle): Integer; stdcall;
begin

end;

//---------------
// Function Returns Non Zero if a Event with the given Name Exists,
// otherwise 0
//---------------
Function EventExists (EventName: PChar): Integer; stdcall;
begin

end;

    {******** Service specific Methods ********}
//---------------
// Function Creates a new Service and Returns the Services Handle
// or 0 on Failure. (Name already exists)
//---------------
Function CreateService (ServiceName: PChar; ServiceProc: TUS_Service): THandle; stdcall;
begin

end;

//---------------
// Function Destroys a Service.
// 0 on success, not 0 on Failure
//---------------
Function DestroyService (hService: THandle): integer; stdcall;
begin

end;
  
//---------------
// Function Calls a Services Proc
// Returns Services Return Value or SERVICE_NOT_FOUND on Failure
//---------------
Function CallService (ServiceName: PChar; wParam, lParam: dWord): integer; stdcall;
begin

end;
   
//---------------
// Function Returns Non Zero if a Service with the given Name Exists,
// otherwise 0                                                      
//---------------
Function ServiceExists (ServiceName: PChar): Integer; stdcall;
begin

end;

end.
