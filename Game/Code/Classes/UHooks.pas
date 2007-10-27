unit UHooks;

{*********************
  THookManager
  Class for saving, managing and calling of Hooks.
  Saves all hookable events and their subscribers
*********************}
interface
uses uPluginDefs, SysUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  //Record that saves info from Subscriber
  PSubscriberInfo = ^TSubscriberInfo;
  TSubscriberInfo = record
    Self: THandle;  //ID of this Subscription (First Word: ID of Subscription; 2nd Word: ID of Hook)
    Next: PSubscriberInfo; //Pointer to next Item in HookChain

    Owner: Integer; //For Error Handling and Plugin Unloading.

    //Here is s/t tricky
    //To avoid writing of Wrapping Functions to Hook an Event with a Class
    //We save a Normal Proc or a Method of a Class
    Case isClass: boolean of
      False: (Proc: TUS_Hook); //Proc that will be called on Event
      True:  (ProcOfClass: TUS_Hook_of_Object);
  end;

  TEventInfo = record
    Name: String[60];                  //Name of Event
    FirstSubscriber:  PSubscriberInfo; //First subscriber in chain
    LastSubscriber:   PSubscriberInfo; //Last " (for easier subscriber adding
  end;

  THookManager = class
    private
      Events: array of TEventInfo;
      SpaceinEvents: Word; //Number of empty Items in Events Array. (e.g. Deleted Items)

      Procedure FreeSubscriber(const EventIndex: Word; const Last, Cur: PSubscriberInfo);
    public
      constructor Create(const SpacetoAllocate: Word);

      Function AddEvent (const EventName: PChar): THandle;
      Function DelEvent (hEvent: THandle): Integer;

      Function AddSubscriber (const EventName: PChar; const Proc: TUS_Hook = nil; const ProcOfClass: TUS_Hook_of_Object = nil): THandle;
      Function DelSubscriber (const hSubscriber: THandle): Integer;

      Function CallEventChain (const hEvent: THandle; const wParam, lParam: LongWord): Integer;
      Function EventExists (const EventName: PChar): Integer;

      Procedure DelbyOwner(const Owner: Integer);
  end;

function HookTest(wParam, lParam: DWord): integer; stdcall;

var
  HookManager: THookManager;

implementation
uses UCore;

//------------
// Create - Creates Class and Set Standard Values
//------------
constructor THookManager.Create(const SpacetoAllocate: Word);
var I: Integer;
begin
  //Get the Space and "Zero" it
  SetLength (Events, SpacetoAllocate);
  For I := 0 to SpacetoAllocate do
    Events[I].Name[1] := chr(0);

  SpaceinEvents := SpacetoAllocate;

  {$IFDEF DEBUG}
    WriteLn('HookManager: Succesful Created.');
  {$ENDIF}
end;

//------------
// AddEvent - Adds an Event and return the Events Handle or 0 on Failure
//------------
Function THookManager.AddEvent (const EventName: PChar): THandle;
var I: Integer;
begin
  Result := 0;

  if (EventExists(EventName) = 0) then
  begin
    If (SpaceinEvents > 0) then
    begin
      //There is already Space available
      //Go Search it!
      For I := 0 to High(Events) do
        If (Events[I].Name[1] = chr(0)) then
        begin //Found Space
          Result := I;
          Dec(SpaceinEvents);
          Break;
        end;

      {$IFDEF DEBUG}
        WriteLn('HookManager: Found Space for Event at Handle: ''' + InttoStr(Result+1) + '');
      {$ENDIF}
    end
    else
    begin //There is no Space => Go make some!
      Result := Length(Events);
      SetLength(Events, Result + 1);
    end;

    //Set Events Data
    Events[Result].Name := EventName;
    Events[Result].FirstSubscriber := nil;
    Events[Result].LastSubscriber := nil;

    //Handle is Index + 1
    Inc(Result);

    {$IFDEF DEBUG}
    WriteLn('HookManager: Add Event succesful: ''' + EventName + '');
    {$ENDIF}
  end
  {$IFDEF DEBUG}
  else
    WriteLn('HookManager: Trying to ReAdd Event: ''' + EventName + '');
  {$ENDIF}
end;

//------------
// DelEvent - Deletes an Event by Handle Returns False on Failure
//------------
Function THookManager.DelEvent (hEvent: THandle): Integer;
var
  Cur, Last: PSubscriberInfo;
begin
  hEvent := hEvent - 1; //Arrayindex is Handle - 1
  Result := -1;


  If (Length(Events) > hEvent) AND (Events[hEvent].Name[1] <> chr(0)) then
  begin //Event exists
    //Free the Space for all Subscribers
    Cur := Events[hEvent].FirstSubscriber;

    While (Cur <> nil) do
    begin
      Last := Cur;
      Cur  := Cur.Next;
      FreeMem(Last, SizeOf(TSubscriberInfo));
    end;

    {$IFDEF DEBUG}
      WriteLn('HookManager: Removed Event succesful: ''' + Events[hEvent].Name + '');
    {$ENDIF}

    //Free the Event
    Events[hEvent].Name[1] := chr(0);
    Inc(SpaceinEvents); //There is one more space for new events
  end

  {$IFDEF DEBUG}
  else
    WriteLn('HookManager: Try to Remove not Existing Event. Handle: ''' + InttoStr(hEvent) + '');
  {$ENDIF}
end;

//------------
// AddSubscriber - Adds an Subscriber to the Event by Name
// Returns Handle of the Subscribtion or 0 on Failure
//------------
Function THookManager.AddSubscriber (const EventName: PChar; const Proc: TUS_Hook; const ProcOfClass: TUS_Hook_of_Object): THandle;
var
  EventHandle: THandle;
  EventIndex:  Cardinal;
  Cur:   PSubscriberInfo;
begin
  Result := 0;

  If (@Proc <> nil) or (@ProcOfClass <> nil) then
  begin
    EventHandle := EventExists(EventName);

    If (EventHandle <> 0) then
    begin
      EventIndex := EventHandle - 1;
      
      //Get Memory
      GetMem(Cur, SizeOf(TSubscriberInfo));

      //Fill it with Data
      Cur.Next := nil;

      //Add Owner
      Cur.Owner := Core.CurExecuted;

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

      //Create Handle (1st Word: Handle of Event; 2nd Word: unique ID
      If (Events[EventIndex].LastSubscriber = nil) then
      begin
        If (Events[EventIndex].FirstSubscriber = nil) then
        begin
          Result := (EventHandle SHL 16);
          Events[EventIndex].FirstSubscriber := Cur;
        end
        Else
        begin
          Result := Events[EventIndex].FirstSubscriber.Self + 1;
        end;
      end
      Else
      begin
        Result := Events[EventIndex].LastSubscriber.Self + 1;
        Events[EventIndex].LastSubscriber.Next := Cur;
      end;

      Cur.Self := Result;

      //Add to Chain
      Events[EventIndex].LastSubscriber := Cur;

      {$IFDEF DEBUG}
          WriteLn('HookManager: Add Subscriber to Event ''' + Events[EventIndex].Name + ''' succesful. Handle: ''' + InttoStr(Result) + ''' Owner: ' + InttoStr(Cur.Owner));
      {$ENDIF}
    end;
  end;
end;

//------------
// FreeSubscriber - Helper for DelSubscriber. Prevents Loss of Chain Items. Frees Memory.
//------------
Procedure THookManager.FreeSubscriber(const EventIndex: Word; const Last, Cur: PSubscriberInfo);
begin
  //Delete from Chain
  If (Last <> nil) then
  begin
    Last.Next := Cur.Next;
  end
  else  //Was first Popup
  begin
    Events[EventIndex].FirstSubscriber := Cur.Next;
  end;

  //Was this Last subscription ?
  If (Cur = Events[EventIndex].LastSubscriber) then
  begin //Change Last Subscriber
    Events[EventIndex].LastSubscriber := Last;
  end;

  //Free Space:
  FreeMem(Cur, SizeOf(TSubscriberInfo));
end;

//------------
// DelSubscriber - Deletes a Subscribtion by Handle, return non Zero on Failure
//------------
Function THookManager.DelSubscriber (const hSubscriber: THandle): Integer;
var
  EventIndex: Cardinal;
  Cur, Last: PSubscriberInfo;
begin
  Result := -1;
  EventIndex := ((hSubscriber AND (High(THandle) xor High(Word))) SHR 16) - 1;

  //Existing Event ?
  If (EventIndex < Length(Events)) AND (Events[EventIndex].Name[1] <> chr(0)) then
  begin
    Result := -2; //Return -1 on not existing Event, -2 on not existing Subscription

    //Search for Subscription
    Cur := Events[EventIndex].FirstSubscriber;
    Last := nil;

    //go through the chain ...
    While (Cur <> nil) do
    begin
      If (Cur.Self = hSubscriber) then
      begin  //Found Subscription we searched for
        FreeSubscriber(EventIndex, Last, Cur);

        {$IFDEF DEBUG}
          WriteLn('HookManager: Del Subscriber from Event ''' + Events[EventIndex].Name + ''' succesful. Handle: ''' + InttoStr(hSubscriber) + '');
        {$ENDIF}

        //Set Result and Break the Loop
        Result := 0;
        Break;
      end;

      Last := Cur;
      Cur := Cur.Next;
    end;

  end;
end;


//------------
// CallEventChain - Calls the Chain of a specified EventHandle
// Returns: -1: Handle doesn't Exist, 0 Chain is called until the End
//------------
Function THookManager.CallEventChain (const hEvent: THandle; const wParam, lParam: LongWord): Integer;
var
  EventIndex: Cardinal;
  Cur: PSubscriberInfo;
  CurExecutedBackup: Integer; //backup of Core.CurExecuted Attribute
begin
  Result := -1;
  EventIndex := hEvent - 1;

  If ((EventIndex <= High(Events)) AND (Events[EventIndex].Name[1] <> chr(0))) then
  begin //Existing Event
    //Backup CurExecuted
    CurExecutedBackup := Core.CurExecuted;

    //Start calling the Chain !!!11
    Cur := Events[EventIndex].FirstSubscriber;
    Result := 0;
    //Call Hooks until the Chain is at the End or breaked
    While ((Cur <> nil) AND (Result = 0)) do
    begin
      //Set CurExecuted
      Core.CurExecuted := Cur.Owner;
      if (Cur.isClass) then
        Result := Cur.ProcOfClass(wParam, lParam)
      else
        Result := Cur.Proc(wParam, lParam);

      Cur := Cur.Next;
    end;

    //Restore CurExecuted
    Core.CurExecuted := CurExecutedBackup;
  end;

  {$IFDEF DEBUG}
    WriteLn('HookManager: Called Chain from Event ''' + Events[EventIndex].Name + ''' succesful. Result: ''' + InttoStr(Result) + '');
  {$ENDIF}
end;

//------------
// EventExists - Returns non Zero if an Event with the given Name exists
//------------
Function THookManager.EventExists (const EventName: PChar): Integer;
var
  I: Integer;
  Name: String[60];
begin
  Result := 0;
  //If (Length(EventName) <
  Name := String(EventName);

  //Sure not to search for empty space
  If (Name[1] <> chr(0)) then
  begin
    //Search for Event
    For I := 0 to High(Events) do
      If (Events[I].Name = Name) then
      begin //Event found
        Result := I + 1;
        Break;
      end;
  end;
end;

//------------
// DelbyOwner - Dels all Subscriptions by a specific Owner. (For Clean Plugin/Module unloading)
//------------
Procedure THookManager.DelbyOwner(const Owner: Integer);
var
  I: Integer;
  Cur, Last: PSubscriberInfo;
begin
  //Search for Owner in all Hooks Chains
  For I := 0 to High(Events) do
  begin
    If (Events[I].Name[1] <> chr(0)) then
    begin
      
      Last := nil;
      Cur  := Events[I].FirstSubscriber;
      //Went Through Chain
      While (Cur <> nil) do
      begin
        If (Cur.Owner = Owner) then
        begin //Found Subscription by Owner -> Delete
          FreeSubscriber(I, Last, Cur);
          If (Last <> nil) then
            Cur := Last.Next
          else
            Cur := Events[I].FirstSubscriber;
        end
        Else
        begin
          //Next Item:
          Last := Cur;
          Cur := Cur.Next;
        end;
      end;
    end;
  end;
end;


function HookTest(wParam, lParam: DWord): integer; stdcall;
begin
  Result := 0; //Don't break the chain
  Core.ShowMessage(CORE_SM_INFO, Integer(PChar(String(PChar(Ptr(lParam))) + ': ' + String(PChar(Ptr(wParam))))));
end;

end.
