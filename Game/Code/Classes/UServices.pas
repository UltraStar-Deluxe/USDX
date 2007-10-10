unit UServices;

interface
uses uPluginDefs, SysUtils;
{*********************
  TServiceManager
  Class for saving, managing and calling of Services.
  Saves all Services and their Procs
*********************}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TServiceName = String[60];
  PServiceInfo = ^TServiceInfo;
  TServiceInfo = record
    Self: THandle; //Handle of this Service
    Hash: Integer; //4 Bit Hash of the Services Name
    Name: TServiceName; //Name of this Service

    Owner: Integer; //If > 0 [DLLMan Pluginindex + 1]; 0 - undefined, On Error Full shutdown, If < 0 [ModuleIndex - 1]  
    
    Next: PServiceInfo; //Pointer to the Next Service in teh list

    //Here is s/t tricky
    //To avoid writing of Wrapping Functions to offer a Service from a Class
    //We save a Normal Proc or a Method of a Class
    Case isClass: boolean of
      False: (Proc: TUS_Service); //Proc that will be called on Event
      True:  (ProcOfClass: TUS_Service_of_Object);
  end;

  TServiceManager = class
    private
      //Managing Service List
      FirstService: PServiceInfo;
      LastService: PServiceInfo;

      //Some Speed improvement by caching the last 4 called Services
      //Most of the time a Service is called multiple times
      ServiceCache: Array[0..3] of PServiceInfo;
      NextCacheItem: Byte;

      //Next Service added gets this Handle:
      NextHandle: THandle;
    public
      Constructor Create;

      Function AddService(const ServiceName: PChar; const Proc: TUS_Service = nil; const ProcofClass: TUS_Service_of_Object = nil): THandle;
      Function DelService(const hService: THandle): integer;

      Function CallService(const ServiceName: PChar; const wParam, lParam: dWord): integer;

      Function NametoHash(const ServiceName: TServiceName): Integer;
      Function ServiceExists(const ServiceName: PChar): Integer;
  end;

var
  ServiceManager: TServiceManager; 

implementation

//------------
// Create - Creates Class and Set Standard Values
//------------
Constructor TServiceManager.Create;
begin
  FirstService := nil;
  LastService := nil;

  ServiceCache[0] := nil;
  ServiceCache[1] := nil;
  ServiceCache[2] := nil;
  ServiceCache[3] := nil;

  NextCacheItem := 0;

  NextHandle := 1;

  {$IFDEF DEBUG}
    WriteLn('ServiceManager: Succesful created!');
  {$ENDIF}
end;

//------------
// Function Creates a new Service and Returns the Services Handle,
// 0 on Failure. (Name already exists)
//------------
Function TServiceManager.AddService(const ServiceName: PChar;  const Proc: TUS_Service; const ProcofClass: TUS_Service_of_Object): THandle;
var
  Cur: PServiceInfo;
begin
  Result := 0;

  If (@Proc <> nil) or (@ProcOfClass <> nil) then
  begin
    If (ServiceExists(ServiceName) = 0) then
    begin //There is a Proc and the Service does not already exist
      //Ok Add it!
      
      //Get Memory
      GetMem(Cur, SizeOf(TServiceInfo));

      //Fill it with Data
      Cur.Next := nil;

      If (@Proc = nil) then
      begin //Use the ProcofClass Method
        Cur.isClass := True;
        Cur.ProcOfClass := ProcofClass;
      end
      else //Use the normal Proc
      begin
        Cur.isClass := False;
        Cur.Proc := Proc;
      end;

      Cur.Self := NextHandle;
      //Zero Name
      Cur.Name := #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0;
      Cur.Name := String(ServiceName);
      Cur.Hash := NametoHash(Cur.Name);

      //Add Service to the List
      If (FirstService = nil) then
        FirstService := Cur;

      If (LastService <> nil) then
        LastService.Next := Cur;

      LastService := Cur;

      {$IFDEF DEBUG}
      WriteLn('ServiceManager: Service added: ''' + ServiceName + ''', Handle: ' + InttoStr(Cur.Self));
      {$ENDIF}

      //Inc Next Handle
      Inc(NextHandle);
    end
    {$IFDEF DEBUG}
    else WriteLn('ServiceManager: Try to readd Service: ' + ServiceName);
    {$ENDIF}
  end;
end;

//------------
// Function Destroys a Service, 0 on success, not 0 on Failure
//------------
Function TServiceManager.DelService(const hService: THandle): integer;
var
  Last, Cur: PServiceInfo;
  I: Integer;
begin
  Result := -1;

  Last := nil;
  Cur := FirstService;

  //Search for Service to Delete
  While (Cur <> nil) do
  begin
    If (Cur.Self = hService) then
    begin //Found Service => Delete it

      //Delete from List
      If (Last = nil) then //Found first Service
        FirstService := Cur.Next
      Else //Service behind the first
        Last.Next := Cur.Next;

      //IF this is the LastService, correct LastService
      If (Cur = LastService) then
        LastService := Last;

      //Search for Service in Cache and delete it if found
      For I := 0 to High(ServiceCache) do
        If (ServiceCache[I] = Cur) then
        begin
          ServiceCache[I] := nil;
        end;

      {$IFDEF DEBUG}
      WriteLn('ServiceManager: Removed Service succesful: ' + Cur.Name);
      {$ENDIF}

      //Free Memory
      Freemem(Cur, SizeOf(TServiceInfo));

      //Break the Loop
      Break;
    end;

    //Go to Next Service
    Last := Cur;
    Cur := Cur.Next;
  end;
end;

//------------
// Function Calls a Services Proc
// Returns Services Return Value or SERVICE_NOT_FOUND on Failure
//------------
Function TServiceManager.CallService(const ServiceName: PChar; const wParam, lParam: dWord): integer;
var
  SExists: Integer;
  Service: PServiceInfo;
begin
  Result := SERVICE_NOT_FOUND;
  SExists := ServiceExists(ServiceName);
  If (SExists <> 0) then
  begin
    Service := ptr(SExists);
    If (Service.isClass) then
      //Use Proc of Class
      Result := Service.ProcOfClass(wParam, lParam)
    Else
      //Use normal Proc
      Result := Service.Proc(wParam, lParam);
  end;

  {$IFDEF DEBUG}
  WriteLn('ServiceManager: Service ''' + ServiceName + ''' called. Result: ' + InttoStr(Result));
  {$ENDIF}
end;

//------------
// Generates the Hash for the given Name
//------------
Function TServiceManager.NametoHash(const ServiceName: TServiceName): Integer;
asm
  { CL: Counter; EAX: Result; EDX: Current Memory Address }
  Mov CL, 14 {Init Counter, Fold 14 Times to became 4 Bytes out of 60}

  Mov EDX, ServiceName {Save Address of String that should be "Hashed"}

  Mov EAX, [EDX]

  @FoldLoop: ADD EDX, 4 {jump 4 Byte(32 Bit) to the next tile }
             ADD EAX, [EDX] {Add the Value of the next 4 Byte of the String to the Hash}

  LOOP @FoldLoop {Fold again if there are Chars Left}
end;


//------------
// Function Returns Non Zero if a Service with the given Name Exists, otherwise 0
//------------
Function TServiceManager.ServiceExists(const ServiceName: PChar): Integer;
var
  Name: TServiceName;
  Hash: Integer;
  Cur: PServiceInfo;
  I: Byte;
begin
  Result := 0;
  // to-do : Write a Metbod (in ASM) to Zero and Add in one turn  (faster then this dirty hack ;)
  //Zero Name:
  Name := #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0;
  //Add Service Name
  Name := String(ServiceName);
  Hash := NametoHash(Name);

  //First of all Look for the Service in Cache
  For I := 0 to High(ServiceCache) do
  begin
    If (ServiceCache[I] <> nil) AND (ServiceCache[I].Hash = Hash) then
    begin
      If (ServiceCache[I].Name = Name) then
      begin //Found Service in Cache
        Result := Integer(ServiceCache[I]);

        {$IFDEF DEBUG}
        WriteLn('ServiceManager: Found Service in Cache: ''' + ServiceName + '''');
        {$ENDIF}

        Break;
      end;
    end;
  end;

  If (Result = 0) then
  begin
    Cur := FirstService;
    While (Cur <> nil) do
    begin
      If (Cur.Hash = Hash) then
      begin
        If (Cur.Name = Name) then
        begin //Found the Service
          Result := Integer(Cur);

          {$IFDEF DEBUG}
          WriteLn('ServiceManager: Found Service in List: ''' + ServiceName + '''');
          {$ENDIF}

          //Add to Cache
          ServiceCache[NextCacheItem] := Cur;
          NextCacheItem := (NextCacheItem + 1) AND 3;
          Break;
        end;
      end;

      Cur := Cur.Next;
    end;
  end;
end;

end.