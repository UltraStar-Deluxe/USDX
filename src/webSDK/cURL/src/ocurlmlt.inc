(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF MULTI_INTERFACE}

type

  tCurlMultiSingleDoneEvent = procedure(Sender:TObject; which:tCurl) of object;
  tCurlMultiSingleDoneCallback = procedure(which:tCurl; UserData:pointer); cdecl;

  tCurlMulti = class ( tCurlBase )
    private
      fMultiHandle: pCurlM;
      fHead:tCurl;
      still_running: LongInt;
      fTimeout: TimeVal;
      rc: LongInt;
      fdread:  tFDSet;
      fdwrite: tFDSet;
      fdexcep: tFDSet;
      maxfd: LongInt;
      fCount:LongInt;
      fOnSingleDone:tCurlMultiSingleDoneEvent;
      fSingleDoneCallback:tCurlMultiSingleDoneCallback;
      fDoneData:pointer;
      fPreserveObjects:boolean;
      function GetSelectTime:LongInt;
      procedure SetSelectTime(ms:LongInt);
      procedure SetSingleDoneCallback(aCallback:tCurlMultiSingleDoneCallback);
      procedure SetOnSingleDone(aEvent:tCurlMultiSingleDoneEvent);
      procedure DoSingleDone(aCurl:tCurl);
      procedure SingleDestroyedExternally(aCurl:TObject);
      function GetObjects(n:LongInt):tCurl;
      procedure AddHandle(aCurl:tCurl);
    public
      constructor Create(aOwner:tComponent); {$IFDEF CURL_COMPONENT}override;{$ENDIF}
      destructor Destroy; override;
      procedure Perform;
      procedure AddObject(aCurl:tCurl);
      procedure DetachObject(aCurl:tCurl);
      function IndexOfObject(aCurl:tCurl):LongInt;
      procedure Clear;
      property Objects[n:integer]:tCurl read GetObjects;
      property Count:LongInt read fCount;
      property SingleDoneCallback:tCurlMultiSingleDoneCallback read fSingleDoneCallback write SetSingleDoneCallback;
      property DoneData:pointer read fDoneData write fDoneData;
    published
      property OnSingleDone:tCurlMultiSingleDoneEvent read fOnSingleDone write SetOnSingleDone;
      property SelectTime:LongInt read GetSelectTime write SetSelectTime;
      property PreserveObjects:boolean read fPreserveObjects write fPreserveObjects;
  end;

{$ENDIF MULTI_INTERFACE}


{$IFDEF CURLOBJ_IMPLEMENTATION}

procedure FreeSinglesList( var aList:tCurl; FreeObjects:boolean);
var
  p1, p2:tCurl;
begin
  p1:=aList;
  while ( p1 <> nil ) do begin
    p1.fMulti:=nil;
    p1.fMultiNotifyDestroying:=nil;
    p2:=tCurl(p1.fNext);
    if FreeObjects then p1.Free;
    p1:=p2;
  end;
  aList:=nil;
end;

Procedure FD_Zero(var fds:fdSet);
{
  Clear the set of filedescriptors
}
begin
  FillChar(fds,sizeof(fdSet),0);
end;

constructor tCurlMulti.Create(aOwner:tComponent);
begin
  inherited Create(aOwner);
  fMultiHandle:=curl_multi_init();
  if ( fMultiHandle = nil ) then FAIL;
  fCount:=0;
  fOnSingleDone:=nil;
  fSingleDoneCallback:=nil;
  fDoneData:=nil;
  fTimeout.tv_sec:=1;
  fTimeout.tv_usec:=0;
  fPreserveObjects:=False;
  fHead:=nil;
  rc:=0;
  FD_ZERO(fdread);
  FD_ZERO(fdwrite);
  FD_ZERO(fdexcep);
  maxfd:=0;
end;


destructor tCurlMulti.Destroy;
begin
  FreeSinglesList(fHead, ( not fPreserveObjects ) );
  curl_multi_cleanup(fMultiHandle);
  inherited Destroy;
end;

procedure tCurlMulti.Clear;
begin
  FreeSinglesList(fHead, ( not fPreserveObjects ) );
end;

function tCurlMulti.GetObjects(n:LongInt):tCurl;
var
  i:LongInt;
begin
  i:=0;
  Result:=fHead;
  while ( Result <> nil ) do begin
    if ( i = n ) then EXIT;
    inc(i);
    Result:=tCurl(Result.fNext);
  end;
end;


procedure tCurlMulti.DetachObject(aCurl:tCurl);
begin
  if ( aCurl <> nil ) and ( aCurl.fMulti = self ) then begin
    if ( aCurl.fPrev <> nil ) then tCurl(aCurl.fPrev).fNext:=aCurl.fNext;
    if ( aCurl.fNext <> nil ) then tCurl(aCurl.fNext).fPrev:=aCurl.fPrev;
    if ( fHead = aCurl ) then fHead:=tCurl(aCurl.fNext);
    aCurl.fMulti:=nil;
    aCurl.fMultiNotifyDestroying:=nil;
    aCurl.fPrev:=nil;
    aCurl.fNext:=nil;
    dec(fCount);
  end;
end;


function tCurlMulti.IndexOfObject(aCurl:tCurl):LongInt;
var
  p:tCurl;
begin
  if ( aCurl <> nil ) then begin
    Result:=0;
    p:=fHead;
    while ( p <> nil ) do begin
      if ( p = aCurl ) then EXIT;
      p:=tCurl(p.fNext);
      inc(Result);
    end;
  end;
  Result:=-1;
end;


procedure tCurlMulti.AddObject(aCurl:tCurl);
var
  PrevItem:tCurl;
begin
  if ( aCurl.fMulti = self ) then EXIT;
  if ( aCurl.fMulti <> nil ) then tCurlMulti(aCurl.fMulti).DetachObject(aCurl);
  aCurl.fMulti:=self;
  aCurl.fMultiNotifyDestroying:={$IFDEF FPC}@{$ENDIF}SingleDestroyedExternally;
  if ( fHead = nil ) then fHead:=aCurl else begin
    PrevItem:=fHead;
    while ( PrevItem.fNext <> nil ) do PrevItem:=tCurl(PrevItem.fNext);
    PrevItem.fNext:=aCurl;
    aCurl.fPrev:=PrevItem;
  end;
  inc(fCount);
  if fBusy then AddHandle(aCurl);
end;


procedure tCurlMulti.DoSingleDone(aCurl:tCurl);
begin
  if ( {$IFNDEF FPC}@{$ENDIF}fOnSingleDone <> nil )
  then fOnSingleDone(self, aCurl)
  else if ( {$IFNDEF FPC}@{$ENDIF}fSingleDoneCallback <> nil )
       then fSingleDoneCallback(aCurl, fDoneData);
end;


procedure tCurlMulti.AddHandle(aCurl:tCurl);
begin
  if ( aCurl.InitTransfer = CURLE_OK ) then begin
    aCurl.fBusy:=True;
    curl_multi_add_handle(fMultiHandle, aCurl.Handle);
    repeat until  ( curl_multi_perform(fMultiHandle, @still_running) <> CURLM_CALL_MULTI_PERFORM );
  end else DoSingleDone(aCurl);
end;


{$IFDEF WIN32}
procedure MultiPerform(p:pointer); cdecl;
{$ELSE}
function MultiPerform(p:pointer):LongInt; cdecl;
{$ENDIF}
var
  MsgRec: pCurlMsgRec;
  MsgCount: LongInt;
  prev_running:LongInt;
  ThisCurl:tCurl;
  aTimeout:TimeVal;
begin
  with tCurlMulti(p) do begin
    fBusy:=True;
    if ( fHead <> nil ) then begin
      ThisCurl:=fHead;
      while ( ThisCurl <> nil ) do begin
        AddHandle(ThisCurl);
        ThisCurl:=tCurl(ThisCurl.fNext);
      end;
      while ( still_running <> 0 ) do begin
        prev_running:=still_running;
        FD_ZERO(fdread);
        FD_ZERO(fdwrite);
        FD_ZERO(fdexcep);
        aTimeout.tv_sec  := fTimeout.tv_sec;
        aTimeout.tv_usec := fTimeout.tv_usec;
        curl_multi_fdset(fMultiHandle, @fdread, @fdwrite, @fdexcep, @maxfd);
        rc := select(maxfd + 1, @fdread, @fdwrite, @fdexcep, @aTimeout);
        if ( rc <> -1 ) then repeat 
        until ( curl_multi_perform(fMultiHandle, @still_running ) <> CURLM_CALL_MULTI_PERFORM );
        if ( prev_running <> still_running ) then repeat
          MsgRec:=curl_multi_info_read(fMultiHandle, @MsgCount);
          if ( MsgRec <> nil )
           and ( MsgRec^.easy_handle <> nil )
            and ( curl_easy_getinfo(MsgRec^.easy_handle, CURLINFO_PRIVATE, ThisCurl) = CURLE_OK )
             and ( ThisCurl <> nil )
          then begin
            ThisCurl.DoneTransfer(MsgRec^.data.result);
            ThisCurl.fBusy:=False;
            curl_multi_remove_handle(fMultiHandle, ThisCurl.Handle);
            DoSingleDone(ThisCurl);
            if ( not fPreserveObjects ) and ( IndexOfObject(ThisCurl) >= 0 ) then ThisCurl.Free;
            end;
        until ( MsgRec = nil ) or ( MsgCount = 0 );
      end;
    end;
    fBusy:=False;
  end;
  {$IFDEF WIN32}
     _endthread();
  {$ELSE}
    Result:=0;
  {$ENDIF}
end;


procedure tCurlMulti.Perform;
begin
  if fThreaded then begin
     fBusy:=True;
      fThread:=_beginthread( @MultiPerform, 0, self );
      while (fBusy) do begin
        Sleep(fWaitInterval);
        if Assigned(fOnWait) and ( {$IFNDEF FPC}@{$ENDIF}fOnWait <> nil ) then fOnWait(self)
        else if Assigned(fWaitCallback) and ( {$IFNDEF FPC}@{$ENDIF}fWaitCallback <> nil ) then fWaitCallback(fWaitData);
      end;
      CloseHandle(fThread);
  end else MultiPerform(self);
end;


function tCurlMulti.GetSelectTime:LongInt;
begin
  Result:=( fTimeout.tv_sec * 1000 ) + ( fTimeout.tv_usec div 1000 );
end;


procedure tCurlMulti.SetSelectTime(ms:LongInt);
begin
  fTimeout.tv_sec:=ms div 1000;
  fTimeout.tv_usec:=( ms mod 1000 ) * 1000;
end;


procedure tCurlMulti.SetSingleDoneCallback(aCallback:tCurlMultiSingleDoneCallback);
begin
  fOnSingleDone:=nil;
  fSingleDoneCallback:=aCallback;
end;


procedure tCurlMulti.SetOnSingleDone(aEvent:tCurlMultiSingleDoneEvent);
begin
  fSingleDoneCallback:=nil;
  fOnSingleDone:=aEvent;
end;


procedure tCurlMulti.SingleDestroyedExternally(aCurl:TObject);
begin
  DetachObject(tCurl(aCurl));
end;

{$ENDIF CURLOBJ_IMPLEMENTATION}

