unit uPluginDefs;
{*********************
  uPluginDefs
  Some Basic Structures and Functions used to communicate with Plugins
  Usable as Delphi Plugin SDK
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  DWORD = LongWord;

  //Compatibility with 64 Bit Systems
  {$IFDEF CPU32}
  TwParam = Integer;
  TlParam = Pointer; //lParam is Used for 32 Bit addresses DWord is large enough
  {$ELSE}
  TwParam = Int64;
  TlParam = Pointer; //lParam used for 64Bit addresses in 64 Bit Systems(FreePascal)
  {$ENDIF}
  //wParam is mainly used for Ordninals
  //lParam is mainly used for Pointers

  //----------------
  // TUS_PluginInfo - Some Infos from Plugin to Core.
  // Send when Plugininfo procedure is Called
  // ---
  // Version Structure:
  // First  Byte: Head Revison
  // Second Byte: Sub Revison
  // Third  Byte: Sub Revision 2
  // Fourth Byte: Letter (For Bug Fix releases. 0 or 'a' .. 'z')
  //----------------
  PUS_PluginInfo = ^TUS_PluginInfo;
  TUS_PluginInfo = record
    cbSize:       Integer;    //Size of this record (usefull if record will be extended in the future)

    Name:         Array [0..31] of Char;      //Name of the Plugin
    Version:      DWord;                      //Version of the Plugin
    Description:  Array [0..127] of Char;     //Description, what does this Plugin do
    Author:       Array [0..31] of Char;      //Author of this Plugin
    AuthorEmail:  Array [0..63] of Char;      //Authors Email
    Homepage:     Array [0..63] of Char;      //Homepage of Plugin/Author
  end;
  AUS_PluginInfo = Array of TUS_PluginInfo;
  PAUS_PluginInfo = ^AUS_PluginInfo;

  //----------------
  // TUS_Hook - Structure of the Hook Function
  // Return 0 if the Hook should be continue,
  // or a non zero Value, if the Hook should be Interuped
  // In this Case the Caller of the Notifier gets the Return Value
  // Return Value Should not be -1
  //----------------
  TUS_Hook            = Function (wParam: TwParam; lParam: TlParam): integer; stdcall;
  TUS_Hook_of_Object  = Function (wParam: TwParam; lParam: TlParam): integer of Object;

  //----------------
  // TUS_Service - Structure of the Service Function
  // This Function is called if the Registered Service is Called
  // Return Value Should not be SERVICE_NOT_FOUND
  //----------------
  TUS_Service           = Function (wParam: TwParam; lParam: TlParam): integer; stdcall;
  TUS_Service_of_Object = Function (wParam: TwParam; lParam: TlParam): integer of Object;

  //----------------
  // TUS_PluginInterface - Structure that Includes all Methods callable
  // from the Plugins
  //----------------
  PUS_PluginInterface = ^TUS_PluginInterface;
  TUS_PluginInterface = record
    {******** Hook specific Methods ********}
    {Function Creates a new Hookable Event and Returns the Handle
     or 0 on Failure. (Name already exists)}
    CreateHookableEvent: Function (EventName: PChar): THandle; stdcall;

    {Function Destroys an Event and Unhooks all Hooks to this Event.
     0 on success, not 0 on Failure}
    DestroyHookableEvent: Function (hEvent: THandle): integer; stdcall;

    {Function start calling the Hook Chain
     0 if Chain is called until the End, -1 if Event Handle is not valid
     otherwise Return Value of the Hook that breaks the Chain}
    NotivyEventHooks: Function (hEvent: THandle; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Hooks an Event by Name.
     Returns Hook Handle on Success, otherwise 0}
    HookEvent: Function (EventName: PChar; HookProc: TUS_Hook): THandle; stdcall;

    {Function Removes the Hook from the Chain
     Returns 0 on Success}
    UnHookEvent: Function (hHook: THandle): Integer; stdcall;

    {Function Returns Non Zero if a Event with the given Name Exists,
     otherwise 0}
    EventExists: Function (EventName: PChar): Integer; stdcall;

    {******** Service specific Methods ********}
    {Function Creates a new Service and Returns the Services Handle
     or 0 on Failure. (Name already exists)}
    CreateService: Function (ServiceName: PChar; ServiceProc: TUS_Service): THandle; stdcall;

    {Function Destroys a Service.
     0 on success, not 0 on Failure}
    DestroyService: Function (hService: THandle): integer; stdcall;

    {Function Calls a Services Proc
     Returns Services Return Value or SERVICE_NOT_FOUND on Failure}
    CallService: Function (ServiceName: PChar; wParam: TwParam; lParam: TlParam): integer; stdcall;

    {Function Returns Non Zero if a Service with the given Name Exists,
     otherwise 0}
    ServiceExists: Function (ServiceName: PChar): Integer; stdcall;
  end;

  //----------------
  //TModuleInfo: Info about Modules. Result of Core/GetModuleInfo
  //----------------
  PModuleInfo = ^TModuleInfo;
  TModuleInfo = record
    Name:         String;
    Version:      LongWord;
    Description:  String;
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
  Func_Load = function (const PInterface: PUS_PluginInterface): Integer; stdcall;

  //Called on Plugins Init. If Non Zero is Returned => abort Loading
  //PInterface is Pointer to PluginInterface
  Func_Init = function (const PInterface: PUS_PluginInterface): Integer; stdcall;

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
// Some Functions to Handle Version DWords
//----------------
Function MakeVersion(const HeadRevision, SubVersion, SubVersion2: Byte; Letter: Char): DWord;
Function VersiontoSting(const Version: DWord): String;


implementation

//--------------
// MakeVersion - Converts 4 Values to a valid Version DWord
//--------------
Function MakeVersion(const HeadRevision, SubVersion, SubVersion2: Byte; Letter: Char): DWord;
begin
  If (letter < 'a') or (Letter > 'z') then
    letter := chr(0);

  Result := (HeadRevision shl 24) or (SubVersion shl 16) or (SubVersion2 shl 8) or Ord(Letter);
end;

//--------------
// VersiontoString - Returns some beauty '1.0.2a' like String
//--------------
Function VersiontoSting(const Version: DWord): String;
begin // to-do : Write VersiontoString without SysUtils depencies
  //Result := InttoStr((ver and $FF000000) shr 24);
  Result := '1.0.1'
end;

end.
