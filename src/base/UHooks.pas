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

unit UHooks;

{*********************
  THookManager
  Class for saving, managing and calling of hooks.
  Saves all hookable events and their subscribers
*********************}
interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  uPluginDefs,
  SysUtils;

type
  //Record that saves info from Subscriber
  PSubscriberInfo = ^TSubscriberInfo;
  TSubscriberInfo = record
    Self: THandle;  // ID of this Subscription (First word: ID of Subscription; 2nd word: ID of Hook)
    Next: PSubscriberInfo; // Pointer to next Item in HookChain

    Owner: integer; //For Error Handling and Plugin Unloading.

    // Here is s/t tricky
    // To avoid writing of Wrapping Functions to Hook an Event with a Class
    // We save a Normal Proc or a Method of a Class
    case isClass: boolean of
      false: (Proc: TUS_Hook); //Proc that will be called on Event
      true:  (ProcOfClass: TUS_Hook_of_Object);
  end;

  TEventInfo = record
    Name:            string[60];      // Name of Event
    FirstSubscriber: PSubscriberInfo; // First subscriber in chain
    LastSubscriber:  PSubscriberInfo; // Last " (for easier subscriber adding)
  end;

  THookManager = class
    private
      Events: array of TEventInfo;
      SpaceinEvents: word; //Number of empty Items in Events Array. (e.g. Deleted Items)

      procedure FreeSubscriber(const EventIndex: word; const Last, Cur: PSubscriberInfo);
    public
      constructor Create(const SpacetoAllocate: word);

      function AddEvent (const EventName: Pchar): THandle;
      function DelEvent (hEvent: THandle): integer;

      function AddSubscriber (const EventName: Pchar; const Proc: TUS_Hook = nil; const ProcOfClass: TUS_Hook_of_Object = nil): THandle;
      function DelSubscriber (const hSubscriber: THandle): integer;

      function CallEventChain (const hEvent: THandle; const wParam: TwParam; lParam: TlParam): integer;
      function EventExists (const EventName: Pchar): integer;

      procedure DelbyOwner(const Owner: integer);
  end;

function HookTest(wParam: TwParam; lParam: TlParam): integer; stdcall;

var
  HookManager: THookManager;

implementation

uses
  ULog,
  UCore;

//------------
// Create - Creates Class and Set Standard Values
//------------
constructor THookManager.Create(const SpacetoAllocate: word);
var
  I: integer;
begin
  inherited Create();

  //Get the Space and "Zero" it
  SetLength (Events, SpacetoAllocate);
  for I := 0 to SpacetoAllocate-1 do
    Events[I].Name[1] := chr(0);

  SpaceinEvents := SpacetoAllocate;

  {$IFDEF DEBUG}
    debugWriteLn('HookManager: Succesful Created.');
  {$ENDIF}
end;

//------------
// AddEvent - Adds an Event and return the Events Handle or 0 on Failure
//------------
function THookManager.AddEvent (const EventName: Pchar): THandle;
var
  I: integer;
begin
  Result := 0;

  if (EventExists(EventName) = 0) then
  begin
    if (SpaceinEvents > 0) then
    begin
      //There is already Space available
      //Go Search it!
      for I := 0 to High(Events) do
        if (Events[I].Name[1] = chr(0)) then
        begin //Found Space
          Result := I;
          Dec(SpaceinEvents);
          Break;
        end;

      {$IFDEF DEBUG}
        debugWriteLn('HookManager: Found Space for Event at Handle: ''' + InttoStr(Result+1) + '');
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
    debugWriteLn('HookManager: Add Event succesful: ''' + EventName + '');
    {$ENDIF}
  end
  {$IFDEF DEBUG}
  else
    debugWriteLn('HookManager: Trying to ReAdd Event: ''' + EventName + '');
  {$ENDIF}
end;

//------------
// DelEvent - Deletes an Event by Handle Returns False on Failure
//------------
function THookManager.DelEvent (hEvent: THandle): integer;
var
  Cur, Last: PSubscriberInfo;
begin
  hEvent := hEvent - 1; //Arrayindex is Handle - 1
  Result := -1;

  if (Length(Events) > hEvent) and (Events[hEvent].Name[1] <> chr(0)) then
  begin //Event exists
    //Free the Space for all Subscribers
    Cur := Events[hEvent].FirstSubscriber;

    while (Cur <> nil) do
    begin
      Last := Cur;
      Cur  := Cur.Next;
      FreeMem(Last, SizeOf(TSubscriberInfo));
    end;

    {$IFDEF DEBUG}
      debugWriteLn('HookManager: Removed Event succesful: ''' + Events[hEvent].Name + '');
    {$ENDIF}

    //Free the Event
    Events[hEvent].Name[1] := chr(0);
    Inc(SpaceinEvents); //There is one more space for new events
  end

  {$IFDEF DEBUG}
  else
    debugWriteLn('HookManager: Try to Remove not Existing Event. Handle: ''' + InttoStr(hEvent) + '');
  {$ENDIF}
end;

//------------
// AddSubscriber - Adds an Subscriber to the Event by Name
// Returns Handle of the Subscribtion or 0 on Failure
//------------
function THookManager.AddSubscriber (const EventName: Pchar; const Proc: TUS_Hook; const ProcOfClass: TUS_Hook_of_Object): THandle;
var
  EventHandle: THandle;
  EventIndex:  integer;
  Cur:         PSubscriberInfo;
begin
  Result := 0;

  if (@Proc <> nil) or (@ProcOfClass <> nil) then
  begin
    EventHandle := EventExists(EventName);

    if (EventHandle <> 0) then
    begin
      EventIndex := EventHandle - 1;

      //Get Memory
      GetMem(Cur, SizeOf(TSubscriberInfo));

      //Fill it with Data
      Cur.Next := nil;

      //Add Owner
      Cur.Owner := Core.CurExecuted;

      if (@Proc = nil) then
      begin //Use the ProcofClass Method
        Cur.isClass := true;
        Cur.ProcOfClass := ProcofClass;
      end
      else //Use the normal Proc
      begin
        Cur.isClass := false;
        Cur.Proc := Proc;
      end;

      //Create Handle (1st word: Handle of Event; 2nd word: unique ID
      if (Events[EventIndex].LastSubscriber = nil) then
      begin
        if (Events[EventIndex].FirstSubscriber = nil) then
        begin
          Result := (EventHandle SHL 16);
          Events[EventIndex].FirstSubscriber := Cur;
        end
        else
        begin
          Result := Events[EventIndex].FirstSubscriber.Self + 1;
        end;
      end
      else
      begin
        Result := Events[EventIndex].LastSubscriber.Self + 1;
        Events[EventIndex].LastSubscriber.Next := Cur;
      end;

      Cur.Self := Result;

      //Add to Chain
      Events[EventIndex].LastSubscriber := Cur;

      {$IFDEF DEBUG}
          debugWriteLn('HookManager: Add Subscriber to Event ''' + Events[EventIndex].Name + ''' succesful. Handle: ''' + InttoStr(Result) + ''' Owner: ' + InttoStr(Cur.Owner));
      {$ENDIF}
    end;
  end;
end;

//------------
// FreeSubscriber - Helper for DelSubscriber. Prevents Loss of Chain Items. Frees Memory.
//------------
procedure THookManager.FreeSubscriber(const EventIndex: word; const Last, Cur: PSubscriberInfo);
begin
  //Delete from Chain
  if (Last <> nil) then
  begin
    Last.Next := Cur.Next;
  end
  else  //Was first Popup
  begin
    Events[EventIndex].FirstSubscriber := Cur.Next;
  end;

  //Was this Last subscription ?
  if (Cur = Events[EventIndex].LastSubscriber) then
  begin //Change Last Subscriber
    Events[EventIndex].LastSubscriber := Last;
  end;

  //Free Space:
  FreeMem(Cur, SizeOf(TSubscriberInfo));
end;

//------------
// DelSubscriber - Deletes a Subscribtion by Handle, return non Zero on Failure
//------------
function THookManager.DelSubscriber (const hSubscriber: THandle): integer;
var
  EventIndex: integer;
  Cur, Last:  PSubscriberInfo;
begin
  Result := -1;
  EventIndex := ((hSubscriber and (High(THandle) xor High(word))) SHR 16) - 1;

  //Existing Event ?
  if (EventIndex < Length(Events)) and (Events[EventIndex].Name[1] <> chr(0)) then
  begin
    Result := -2; //Return -1 on not existing Event, -2 on not existing Subscription

    //Search for Subscription
    Cur := Events[EventIndex].FirstSubscriber;
    Last := nil;

    //go through the chain ...
    while (Cur <> nil) do
    begin
      if (Cur.Self = hSubscriber) then
      begin  //Found Subscription we searched for
        FreeSubscriber(EventIndex, Last, Cur);

        {$IFDEF DEBUG}
          debugWriteLn('HookManager: Del Subscriber from Event ''' + Events[EventIndex].Name + ''' succesful. Handle: ''' + InttoStr(hSubscriber) + '');
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
function THookManager.CallEventChain (const hEvent: THandle; const wParam: TwParam; lParam: TlParam): integer;
var
  EventIndex:        integer;
  Cur:               PSubscriberInfo;
  CurExecutedBackup: integer; // backup of Core.CurExecuted Attribute
begin
  Result := -1;
  EventIndex := hEvent - 1;

  if ((EventIndex <= High(Events)) and (Events[EventIndex].Name[1] <> chr(0))) then
  begin //Existing Event
    //Backup CurExecuted
    CurExecutedBackup := Core.CurExecuted;

    //Start calling the Chain !!!11
    Cur := Events[EventIndex].FirstSubscriber;
    Result := 0;
    //Call Hooks until the Chain is at the End or breaked
    while ((Cur <> nil) and (Result = 0)) do
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
    debugWriteLn('HookManager: Called Chain from Event ''' + Events[EventIndex].Name + ''' succesful. Result: ''' + InttoStr(Result) + '');
  {$ENDIF}
end;

//------------
// EventExists - Returns non Zero if an Event with the given Name exists
//------------
function THookManager.EventExists (const EventName: Pchar): integer;
var
  I:    integer;
  Name: string[60];
begin
  Result := 0;
  //if (Length(EventName) <
  Name := string(EventName);

  //Sure not to search for empty space
  if (Name[1] <> chr(0)) then
  begin
    //Search for Event
    for I := 0 to High(Events) do
      if (Events[I].Name = Name) then
      begin //Event found
        Result := I + 1;
        Break;
      end;
  end;
end;

//------------
// DelbyOwner - Dels all Subscriptions by a specific Owner. (For Clean Plugin/Module unloading)
//------------
procedure THookManager.DelbyOwner(const Owner: integer);
var
  I:         integer;
  Cur, Last: PSubscriberInfo;
begin
  //Search for Owner in all Hooks Chains
  for I := 0 to High(Events) do
  begin
    if (Events[I].Name[1] <> chr(0)) then
    begin

      Last := nil;
      Cur  := Events[I].FirstSubscriber;
      //Went Through Chain
      while (Cur <> nil) do
      begin
        if (Cur.Owner = Owner) then
        begin //Found Subscription by Owner -> Delete
          FreeSubscriber(I, Last, Cur);
          if (Last <> nil) then
            Cur := Last.Next
          else
            Cur := Events[I].FirstSubscriber;
        end
        else
        begin
          //Next Item:
          Last := Cur;
          Cur := Cur.Next;
        end;
      end;
    end;
  end;
end;

function HookTest(wParam: TwParam; lParam: TlParam): integer; stdcall;
begin
  Result := 0; //Don't break the chain
  Core.ShowMessage(CORE_SM_INFO, Pchar(string(Pchar(Pointer(lParam))) + ': ' + string(Pchar(Pointer(wParam)))));
end;

end.
