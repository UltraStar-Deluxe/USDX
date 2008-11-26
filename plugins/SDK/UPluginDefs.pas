unit uPluginDefs;
{*********************
  uPluginDefs
  Some basic structures and functions used to communicate with plugins
  Usable as Delphi plugin SDK
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  dword = LongWord;

  //Compatibility with 64 Bit Systems
  {$IFDEF CPU32}
  TwParam = integer;
  TlParam = pointer; //lParam is used for 32 bit addresses. dword is large enough
  {$ELSE}
  TwParam = int64;
  TlParam = pointer; //lParam used for 64 bit addresses in 64 bit systems (FreePascal)
  {$ENDIF}
  //wParam is mainly used for ordinals
  //lparam is mainly used for pointers

  //----------------
  // TUS_PluginInfo - some infos from plugin to core.
  // Send when Plugininfo procedure is called
  // ---
  // Version structure:
  // First  byte: Head Revison
  // Second byte: Sub Revison
  // Third  byte: Sub Revision 2
  // Fourth byte: Letter (For Bug Fix releases. 0 or 'a' .. 'z')
  //----------------
  PUS_PluginInfo = ^TUS_PluginInfo;
  TUS_PluginInfo = record
    cbSize:       integer;    //Size of this record (usefull if record will be extended in the future)

    Name:         array [0..31] of char;      //Name of the Plugin
    Version:      dword;                      //Version of the Plugin
    Description:  array [0..127] of char;     //Description, what does this Plugin do
    Author:       array [0..31] of char;      //Author of this Plugin
    AuthorEmail:  array [0..63] of char;      //Authors Email
    Homepage:     array [0..63] of char;      //Homepage of Plugin/Author
  end;
  AUS_PluginInfo = array of TUS_PluginInfo;
  PAUS_PluginInfo = ^AUS_PluginInfo;

  //----------------
  // TUS_Hook - Structure of the Hook function
  // Return 0 if the Hook should be continue,
  // or a non zero Value, if the Hook should be Interuped
  // In this Case the Caller of the Notifier gets the Return Value
  // Return Value Should not be -1
  //----------------
  TUS_Hook            = function (wParam: TwParam; lParam: TlParam): integer; stdcall;
  TUS_Hook_of_Object  = function (wParam: TwParam; lParam: TlParam): integer of Object;

  //----------------
  // TUS_Service - Structure of the Service function
  // This function is called if the Registered Service is Called
  // Return Value Should not be SERVICE_NOT_FOUND
  //----------------
  TUS_Service           = function (wParam: TwParam; lParam: TlParam): integer; stdcall;
  TUS_Service_of_Object = function (wParam: TwParam; lParam: TlParam): integer of Object;

  //----------------
  // TUS_PluginInterface - Structure that Includes all Methods callable
  // from the Plugins
  //----------------
  PUS_PluginInterface = ^TUS_PluginInterface;
  TUS_PluginInterface = record
    {******** Hook specific Methods ********}
    {Function Creates a new Hookable Event and Returns the Handle
     or 0 on Failure. (Name already exists)}
    CreateHookableEvent: function (EventName: PChar): THandle; stdcall;

    {Function Destroys an Event and Unhooks all Hooks to this Event.
     0 on success, not 0 on Failure}
    DestroyHookableEvent: function (hEvent: THandle): integer; stdcall;

    {Function start calling the Hook Chain
     0 if Chain is called until the End, -1 if Event Handle is not valid
     otherwise Return Value of the Hook that breaks the Chain}
    NotivyEventHooks: function (hEvent: THandle; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Hooks an Event by Name.
     Returns Hook Handle on Success, otherwise 0}
    HookEvent: function (EventName: PChar; HookProc: TUS_Hook): THandle; stdcall;

    {Function Removes the Hook from the Chain
     Returns 0 on Success}
    UnHookEvent: function (hHook: THandle): integer; stdcall;

    {Function Returns Non Zero if a Event with the given Name Exists,
     otherwise 0}
    EventExists: function (EventName: PChar): integer; stdcall;

    {******** Service specific Methods ********}
    {Function Creates a new Service and Returns the Services Handle
     or 0 on Failure. (Name already exists)}
    CreateService: function (ServiceName: PChar; ServiceProc: TUS_Service): THandle; stdcall;

    {Function Destroys a Service.
     0 on success, not 0 on Failure}
    DestroyService: function (hService: THandle): integer; stdcall;

    {Function Calls a Services Proc
     Returns Services Return Value or SERVICE_NOT_FOUND on Failure}
    CallService: function (ServiceName: PChar; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Returns Non Zero if a Service with the given Name Exists,
     otherwise 0}
    ServiceExists: function (ServiceName: PChar): integer; stdcall;
  end;

  //----------------
  //TModuleInfo: Info about Modules. Result of Core/GetModuleInfo
  //----------------
  PModuleInfo = ^TModuleInfo;
  TModuleInfo = record
    Name:         string;
    Version:      LongWord;
    Description:  string;
  end;
  AModuleInfo = array of TModuleInfo;

  //----------------
  // Procs that should be exported by Plugin Dlls
  //----------------
  //Procedure is called to check if this is USDx Plugin
  //Info is Pointer to this Plugins Info. Size is already set. Don't write over this limit
  Proc_PluginInfo = procedure (Info: PUS_PluginInfo); stdcall;

  //Called on Plugins Load. If Non Zero is Returned => abort Loading
  //PInterface is Pointer to PluginInterface
  Func_Load = function (const PInterface: PUS_PluginInterface): integer; stdcall;

  //Called on Plugins Init. If Non Zero is Returned => abort Loading
  //PInterface is Pointer to PluginInterface
  Func_Init = function (const PInterface: PUS_PluginInterface): integer; stdcall;

  //Called on Plugins Deinit.
  //PInterface is Pointer to PluginInterface
  Proc_DeInit = procedure (const PInterface: PUS_PluginInterface); stdcall;

//----------------
// Some Default Constants
//----------------
const
  {Returned if Service is not found from CallService}
  SERVICE_NOT_FOUND = LongInt($80000000);

  //for use in Service 'Core/ShowMessage' lParam(Symbol)
  CORE_SM_NOSYMBOL= 0;
  CORE_SM_ERROR   = 1;
  CORE_SM_WARNING = 2;
  CORE_SM_INFO    = 3;

//----------------
// Some functions to Handle Version dwords
//----------------
function MakeVersion(const HeadRevision, SubVersion, SubVersion2: byte; Letter: char): dword;
function VersionToString(const Version: dword): string;

implementation

//--------------
// MakeVersion - converts 4 values to a valid version dword
//--------------
function MakeVersion(const HeadRevision, SubVersion, SubVersion2: byte; Letter: char): dword;
begin
  if(letter < 'a') or (Letter > 'z') then
    letter := chr(0);

  Result := (HeadRevision shl 24) or (SubVersion shl 16) or (SubVersion2 shl 8) or Ord(Letter);
end;

//--------------
// VersiontoString - Returns some beauty '1.0.2a' like string
//--------------
function VersionToString(const Version: dword): string;
begin // to-do : Write VersiontoString without SysUtils dependence
  //Result := InttoStr((ver and $FF000000) shr 24);
  Result := '1.0.1'
end;

end.
