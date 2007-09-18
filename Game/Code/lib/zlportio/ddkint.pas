{ -----------------------------------------------------------------------------}
{ Copyright 2000-2001, Zloba Alexander.  All Rights Reserved.                  }
{ This unit can be freely used and distributed in commercial and private       }
{ environments, provided this notice is not modified in any way.               }
{ -----------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions at}
{   zal@specosoft.com (Zloba Alexander)                                        }
{ You can always find the latest version of this unit at:                      }
{   http://www.specosoft.com                                                   }

{ -----------------------------------------------------------------------------}
{ Date last modified:  08/10/2001                                              }
{ -----------------------------------------------------------------------------}
{ Description:                                                                 }
{   This unit include service function to work with NT drivers and some        }
{    constant from ntddk.h                                                     }
{------------------------------------------------------------------------------}
{ Revision History:                                                            }
{ 1.00:  + First public release                                                }
{ 1.10:  + added compiler directives for correct compilation                   }
{ 1.20:  + optimized code                                                      }
{ 1.30:  + added constant for compatibility with delphi 3.0                    }
{------------------------------------------------------------------------------}

{$A-,H-}
unit ddkint;

interface
uses
    windows,
    winsvc;

function CTL_CODE(const DeviceType,Func,Method,Access:Cardinal):cardinal;

const
 FILE_DEVICE_BEEP               = $00000001;
 FILE_DEVICE_CD_ROM             = $00000002;
 FILE_DEVICE_CD_ROM_FILE_SYSTEM = $00000003;
 FILE_DEVICE_CONTROLLER         = $00000004;
 FILE_DEVICE_DATALINK           = $00000005;
 FILE_DEVICE_DFS                = $00000006;
 FILE_DEVICE_DISK               = $00000007;
 FILE_DEVICE_DISK_FILE_SYSTEM   = $00000008;
 FILE_DEVICE_FILE_SYSTEM        = $00000009;
 FILE_DEVICE_INPORT_PORT        = $0000000a;
 FILE_DEVICE_KEYBOARD           = $0000000b;
 FILE_DEVICE_MAILSLOT           = $0000000c;
 FILE_DEVICE_MIDI_IN            = $0000000d;
 FILE_DEVICE_MIDI_OUT           = $0000000e;
 FILE_DEVICE_MOUSE              = $0000000f;
 FILE_DEVICE_MULTI_UNC_PROVIDER = $00000010;
 FILE_DEVICE_NAMED_PIPE         = $00000011;
 FILE_DEVICE_NETWORK            = $00000012;
 FILE_DEVICE_NETWORK_BROWSER    = $00000013;
 FILE_DEVICE_NETWORK_FILE_SYSTEM= $00000014;
 FILE_DEVICE_NULL               = $00000015;
 FILE_DEVICE_PARALLEL_PORT      = $00000016;
 FILE_DEVICE_PHYSICAL_NETCARD   = $00000017;
 FILE_DEVICE_PRINTER            = $00000018;
 FILE_DEVICE_SCANNER            = $00000019;
 FILE_DEVICE_SERIAL_MOUSE_PORT  = $0000001a;
 FILE_DEVICE_SERIAL_PORT        = $0000001b;
 FILE_DEVICE_SCREEN             = $0000001c;
 FILE_DEVICE_SOUND              = $0000001d;
 FILE_DEVICE_STREAMS            = $0000001e;
 FILE_DEVICE_TAPE               = $0000001f;
 FILE_DEVICE_TAPE_FILE_SYSTEM   = $00000020;
 FILE_DEVICE_TRANSPORT          = $00000021;
 FILE_DEVICE_UNKNOWN            = $00000022;
 FILE_DEVICE_VIDEO              = $00000023;
 FILE_DEVICE_VIRTUAL_DISK       = $00000024;
 FILE_DEVICE_WAVE_IN            = $00000025;
 FILE_DEVICE_WAVE_OUT           = $00000026;
 FILE_DEVICE_8042_PORT          = $00000027;
 FILE_DEVICE_NETWORK_REDIRECTOR = $00000028;
 FILE_DEVICE_BATTERY            = $00000029;
 FILE_DEVICE_BUS_EXTENDER       = $0000002a;
 FILE_DEVICE_MODEM              = $0000002b;
 FILE_DEVICE_VDM                = $0000002c;
 FILE_DEVICE_MASS_STORAGE       = $0000002d;
 FILE_DEVICE_SMB                = $0000002e;
 FILE_DEVICE_KS                 = $0000002f;
 FILE_DEVICE_CHANGER            = $00000030;
 FILE_DEVICE_SMARTCARD          = $00000031;
 FILE_DEVICE_ACPI               = $00000032;
 FILE_DEVICE_DVD                = $00000033;
 FILE_DEVICE_FULLSCREEN_VIDEO   = $00000034;
 FILE_DEVICE_DFS_FILE_SYSTEM    = $00000035;
 FILE_DEVICE_DFS_VOLUME         = $00000036;
 FILE_DEVICE_SERENUM            = $00000037;
 FILE_DEVICE_TERMSRV            = $00000038;
 FILE_DEVICE_KSEC               = $00000039;

 FILE_DEVICE_KRNLDRVR           = $80ff;

 METHOD_BUFFERED   =              0;
 METHOD_IN_DIRECT  =              1;
 METHOD_OUT_DIRECT =              2;
 METHOD_NEITHER    =              3;

 FILE_ANY_ACCESS     =            0;
 FILE_SPECIAL_ACCESS =   (FILE_ANY_ACCESS);
 FILE_READ_ACCESS    =      ( $0001 );    // file & pipe
 FILE_WRITE_ACCESS   =      ( $0002 );    // file & pipe

 {$IFDEF VER100 or VER110}
 // for compatibilty with delphi 3.0
const
  SERVICE_KERNEL_DRIVER         = $00000001;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_ERROR_NORMAL          = $00000001;

{$ENDIF}

function driverstart(const name:pchar):integer;
function driverstop(const name:pchar):integer;

// for this function must have Administrators or Power users rigths
function driverinstall(const path,name:pchar):integer;
function driverremove(const name:pchar):integer;


// exlpanation function
function messagestring(const error:integer):string;

implementation

function CTL_CODE(const DeviceType,Func,Method,Access:Cardinal):cardinal;
begin
 Result := DeviceType shl 16 or Access shl 14 or Func shl 2 or Method;
end;


function driverinstall(const path,name:pchar):integer;
var hService: SC_HANDLE;
    hSCMan  : SC_HANDLE;
begin

  Result := 0;

  hSCMan := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCMan = 0 then  begin
   result := getlasterror;
   exit;
  end;

  hService := CreateService(hSCMan, name,name,
              SERVICE_ALL_ACCESS, SERVICE_KERNEL_DRIVER, SERVICE_DEMAND_START,
              SERVICE_ERROR_NORMAL, path,
              nil, nil, nil, nil, nil);

  if (hService = 0) then begin
    result := getlasterror;
    CloseServiceHandle(hSCMan);
    exit;
  end
  else
    CloseServiceHandle(hService);
  CloseServiceHandle(hSCMan);
end;

function driverstart(const name:pchar):integer;
var
  hService: SC_HANDLE;
  hSCMan  : SC_HANDLE;
  args:pchar;
begin

  hSCMan := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if hSCMan = 0 then  begin
   result := getlasterror;
   exit;
  end;

  // get a handle to the service
  hService := OpenService(hSCMan, name, SERVICE_START);
  if hService <> 0 then Begin
     // start the driver
    args := nil;
    Result := 0;
    if integer(StartService(hService, 0, args ))=0 then
      result := getlasterror;
    CloseServiceHandle(hService);
  end
  else
    result := getlasterror;
  CloseServiceHandle(hSCMan);
end;

function driverstop(const name:pchar):integer;
Var
  serviceStatus: TServiceStatus;
  hService: SC_HANDLE;
  hSCMan  : SC_HANDLE;
begin

  hSCMan := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if hSCMan = 0 then  begin
   result := getlasterror;
   exit;
  end;

  // get a handle to the service
  hService := OpenService(hSCMan, Name, SERVICE_STOP);
  if hService <> 0 then Begin
     // start the driver
    Result := 0;
    if integer(ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus))=0 then
      result := getlasterror;
    CloseServiceHandle(hService);
  end
  else
    result := getlasterror;
  CloseServiceHandle(hSCMan);
end;

function driverremove(const name:pchar):integer;
Var
  hService: SC_HANDLE;
  hSCMan  : SC_HANDLE;
begin

  hSCMan := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCMan = 0 then  begin
    result := getlasterror;
    exit;
  end;

  // get a handle to the service
  hService := OpenService(hSCMan, Name, SERVICE_ALL_ACCESS);
  if hService <> 0 then Begin
    // remove driver description from the registry
    Result := 0;
    if integer(DeleteService(hService)) = 0 then
      result := getlasterror;
    CloseServiceHandle(hService);
  end
  else
    result := getlasterror;
  CloseServiceHandle(hSCMan);
end;

function messagestring(const error:integer):string;
var p:pchar;
begin
  GetMem(p, 200);
  FillChar(p^, 200, 0);
  formatmessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,error,0,p,199,nil);
  Result := p;
  freemem(p,200);
end;

end.
